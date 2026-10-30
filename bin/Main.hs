{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Attulus.Source

import Prelude

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory
import System.Environment
import System.FilePath

import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Default
import qualified Data.HashMap.Lazy as HM
import Data.List (minimumBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Fail
import Effectful.Log
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, put)

--------------------------------------- IO -------------------------------------

main :: IO ()
main = runEff
  . (runFail >=> either (liftIO . print) pure)
  . withConfig
  . withLog
  . withSaved
  . withIgnorelist
  . runConcurrent
  $ do
    logInfo_ "Start Attulus"
    artists <- asks configMusicbrainz >>= \r -> runReader r requestList
    withOldest artists processArtist

withIgnorelist
  :: (IOE :> es, Fail :> es, Log :> es) => Eff (Reader Ignorelist : es) a -> Eff es a
withIgnorelist run = do
  f <-
    liftIO (lookupEnv "XDG_CONFIG_HOME")
      >>= maybe (fail "No XDG_CONFIG_HOME env variable set") pure
  content <- liftIO $ readFile (f </> "attulus" </> "ignorelist")
  runReader (Ignorelist $ lines content) run

withConfig
  :: (IOE :> es, Fail :> es) => Eff (Reader Config : es) a -> Eff es a
withConfig run = do
  configDir <-
    liftIO (lookupEnv "XDG_CONFIG_HOME")
      >>= maybe (fail "No XDG_CONFIG_HOME env variable set") pure
  let configFile = configDir </> "attulus" </> "config.yaml"
  config <- liftIO (Y.decodeFileEither configFile) >>= either (fail . show) pure
  runReader config run

withSaved
  :: (IOE :> es, Fail :> es) => Eff (State Artists : es) a -> Eff es ()
withSaved run = do
  dataDir <-
    liftIO (lookupEnv "XDG_DATA_HOME")
      >>= maybe (fail "No XDG_DATA_HOME env variable set") pure
  let dataFile = dataDir </> "attulus" </> "data.yaml"

  exists <- liftIO (doesFileExist dataFile)
  artists <-
    if not exists
      then pure def
      else liftIO (Y.decodeFileEither dataFile) >>= either (fail . show) pure
  new <- execState artists run
  liftIO $ Y.encodeFile dataFile new

withOldest
  :: (Log :> es, State Artists :> es) => Artists -> Eff (State Artist : es) a -> Eff es ()
withOldest (Artists new) run = do
  logInfo_ "Fetching collection"
  Artists old <- get
  let merged = HM.unionWith (\l' r' -> l'{artistDisplayTitle = artistDisplayTitle r'}) old new
      (key, oldest) =
        minimumBy (\(_, a) (_, b) -> compare (artistLastUpdate a) (artistLastUpdate b)) $
          HM.toList merged
  updated <- execState oldest run
  put (Artists $ HM.insert key updated merged)
  pure ()

withLog :: (IOE :> es, Reader Config :> es) => Eff (Log : es) a -> Eff es a
withLog run = do
  level <- asks (attulusLogLevel . configAttulus)
  logger <-
    liftIO $
      mkLogger "main" (\LogMessage{..} -> T.putStrLn (T.show lmTime <> ": " <> lmMessage))
  runLog "attulus" logger level run

processArtist
  :: ( Concurrent :> es
     , Reader Config :> es
     , Reader Ignorelist :> es
     , IOE :> es
     , Log :> es
     , Fail :> es
     , State Artist :> es
     )
  => Eff es ()
processArtist = do
  gets artistDisplayTitle >>= logInfo_ . ("Processing " <>) . T.show
  Config{..} <- ask
  shared <- get >>= newTVarIO
  entries <- newTQueueIO
  total <-
    requestArtist
      >>= ( fix \go -> \case
              (n, []) -> pure n
              (n, x : xs) -> do
                _ <-
                  forkFinally
                    (handleLink shared entries x)
                    ( \e ->
                        either (logAttention_ . T.show) pure e >> atomically (writeTQueue entries (Left ()))
                    )
                go (succ n, xs)
          )
        . (0 :: Int,)
        . unLinks

  fix
    ( \go -> \case
        0 -> pure ()
        n ->
          atomically (readTQueue entries) >>= \case
            Left () -> go (pred n)
            Right entry -> pushEntry configMiniflux entry >> go n
    )
    total

  now <- round <$> liftIO getPOSIXTime
  combined <- readTVarIO shared
  put combined{artistLastUpdate = now}

handleLink
  :: forall es
   . ( Concurrent :> es
     , IOE :> es
     , Log :> es
     , Reader Config :> es
     , Fail :> es
     , Reader Ignorelist :> es
     )
  => TVar Artist -> TQueue (Either () Entry) -> Link -> Eff es ()
handleLink shared entries Link{..}
  | typ == "discogs" = asks configDiscogs >>= mkHandle handleDiscogs "discogs"
  | typ == "ticketmaster" =
      asks configTicketMaster >>= mkHandle handleTicketMaster "ticketmaster"
  | otherwise = do
      Ignorelist list <- ask
      unless (typ `elem` list) $
        atomically
          ( writeTQueue entries $
              Right (Entry ("Unknown type " <> typ) "linkUrl" "Musicbrainz" "" linkType)
          )
 where
  typ = toLower <$> linkType
  mkHandle :: LinkHandler c -> String -> c -> Eff es ()
  mkHandle f name = f linkUrl \cb -> atomically do
    Artist{..} <- readTVar shared
    let (newEntries, newArtist) = HM.alterF (cb artistDisplayTitle) name artistNews
    writeTVar shared (Artist{artistNews = newArtist, ..})
    forM_ newEntries (writeTQueue entries . Right)

-- v2 api has no clean way to get concerts by legacy artist id
handleTicketMaster :: LinkHandler TicketMasterConfig
handleTicketMaster _ _ _ = pure ()
