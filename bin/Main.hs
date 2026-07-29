{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude

import PackageInfo_attulus

import GHC.Generics
import System.Environment
import System.FilePath

import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Bool
import Data.ByteString.Char8 (ByteString, fromStrict, pack)
import Data.Char
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Lazy as HM
import Data.List (intercalate, sortBy)
import Data.Version
import qualified Data.Yaml as Y

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Fix (fix)
import Control.Monad.Trans

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory

import Network.Connection (TLSSettings (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data Config = Config {configMiniflux :: MinifluxConfig, configDiscogs :: DiscogsConfig}
  deriving (Generic, Show)
data MinifluxConfig = MinifluxConfig {minifluxKey :: String, minifluxFeed :: String}
  deriving (Generic, Show)
data DiscogsConfig = DiscogsConfig {discogsKey :: String, discogsList :: String}
  deriving (Generic, Show)

newtype Artists = Artists {unArtists :: HM.HashMap Int Artist} deriving Show
data Artist = Artist
  { artistId :: Int
  , artistLastUpdate :: Int
  , artistsReleases :: Releases
  , artistDisplayTitle :: String
  }
  deriving (Show, Generic)
newtype Releases = Releases {unReleases :: HM.HashMap Int Release} deriving Show
data Release = Release
  { releaseId :: Int
  , releaseType :: String
  , releaseRole :: String
  , releaseTitle :: String
  , releaseYear :: Int
  , releaseArtist :: String
  }
  deriving (Generic, Show)

data Entry = Entry
  { entryTitle :: String
  , entryUrl :: String
  , entryAuthor :: String
  , entryContent :: String
  , entryPublishedAt :: Int
  , entryStatus :: String
  , entryStarred :: Bool
  , entryTags :: [String]
  , entryExternalId :: String
  , entryCommentsUrl :: String
  }
  deriving (Show, Generic)
data ListResponse = ListResponse Int Int Artists deriving Show
data ArtistResponse = ArtistResponse Int Int Releases deriving Show

opts :: Options
opts = defaultOptions{fieldLabelModifier = camelFrom2, rejectUnknownFields = True}

camelFrom2 :: String -> String
camelFrom2 "" = ""
camelFrom2 (x : xs) = if isUpper x then toLower x : xs else camelFrom2 xs

underscoreFrom2 :: String -> String
underscoreFrom2 "" = ""
underscoreFrom2 (x : xs) = if isUpper x then toLower x : underscoreRest xs else underscoreFrom2 xs
 where
  underscoreRest "" = ""
  underscoreRest (y : ys) = if isUpper y then '_' : toLower y : underscoreRest ys else y : underscoreRest ys

instance Default Artists where
  def = Artists HM.empty

instance Default Artist where
  def = Artist def def def def

instance Default Releases where
  def = Releases HM.empty

instance Default Release where
  def = Release def def def def def def

instance FromJSON Config where
  parseJSON = genericParseJSON opts

instance FromJSON MinifluxConfig where
  parseJSON = genericParseJSON opts

instance FromJSON DiscogsConfig where
  parseJSON = genericParseJSON opts

instance FromJSON Artists where
  parseJSON = withArray "artists" \arr -> do
    kvs <- traverse (fmap (\a -> (artistId a, a)) . parseJSON) arr
    pure $ Artists $ foldl' (\m (k, v) -> HM.insert k v m) HM.empty kvs

instance FromJSON Artist where
  parseJSON = withObject "artist" \w -> do
    artistId <- w .:? "id" .!= 0
    artistLastUpdate <- w .:? "last_update" .!= 0
    artistsReleases <- w .:? "releases" .!= Releases HM.empty
    artistDisplayTitle <- w .:? "display_title" .!= ""
    pure Artist{..}

instance FromJSON Releases where
  parseJSON = withArray "releases" \arr -> do
    kvs <- traverse (fmap (\a -> (releaseId a, a)) . parseJSON) arr
    pure $ Releases $ foldl' (\m (k, v) -> HM.insert k v m) HM.empty kvs

instance FromJSON Release where
  parseJSON (Object w) = do
    releaseId <- w .:? "id" .!= 0
    releaseTitle <- w .:? "title" .!= ""
    releaseType <- w .:? "type" .!= ""
    releaseRole <- w .:? "role" .!= ""
    releaseYear <- w .:? "year" .!= 0
    releaseArtist <- w .:? "artist" .!= ""
    pure Release{..}
  parseJSON (Number i) = pure $ def{releaseId = round i}
  parseJSON _ = fail "Release type mismatch: should object or int"

instance ToJSON Entry where
  toJSON = genericToJSON opts{fieldLabelModifier = underscoreFrom2}

instance ToJSON Artists where
  toJSON (Artists hm) = toJSON (HM.elems hm)

instance ToJSON Artist where
  toJSON Artist{..} =
    object
      [ "display_title" .= artistDisplayTitle
      , "id" .= artistId
      , "last_update" .= artistLastUpdate
      , "releases" .= artistsReleases
      ]

instance ToJSON Releases where
  toJSON (Releases hm) = toJSON (HM.elems $ fmap releaseId hm)

instance FromJSON ListResponse where
  parseJSON = withObject "list_response" \w -> do
    artists <- w .: "items"
    (page, pages) <-
      w .:? "pagination" .!= object ["page" .= (1 :: Int), "pages" .= (1 :: Int)] >>= withObject "pagination" \v -> do
        (,) <$> v .: "page" <*> v .: "pages"
    pure $ ListResponse page pages artists

instance FromJSON ArtistResponse where
  parseJSON = withObject "artist_response" \w -> do
    artists <- w .: "releases"
    (page, pages) <-
      w .:? "pagination" .!= object ["page" .= (1 :: Int), "pages" .= (1 :: Int)] >>= withObject "pagination" \v -> do
        (,) <$> v .: "page" <*> v .: "pages"
    pure $ ArtistResponse page pages artists

userAgent :: ByteString
userAgent = pack $ capitalize name <> "RssBot/" <> showVersion version <> " +" <> homepage
 where
  capitalize "" = ""
  capitalize (x : xs) = toUpper x : xs

main :: IO ()
main =
  either print pure =<< runExceptT do
    dataDir <-
      lift (lookupEnv "XDG_DATA_HOME") >>= maybe (fail "No XDG_DATA_HOME env variable set") pure
    let dataFile = dataDir </> "attulus" </> "data.yaml"

    (Config{..}, saved) <- loadConfig dataFile
    artists <- requestList configDiscogs
    let combined = joinArtists artists saved
        Artists targets = takeOldest 1 combined
    toSave <- lift $ foldM (processArtist configDiscogs configMiniflux) saved targets
    lift $ Y.encodeFile dataFile toSave

processArtist :: DiscogsConfig -> MinifluxConfig -> Artists -> Artist -> IO Artists
processArtist configDiscogs configMiniflux saved artist =
  runExceptT (requestReleases configDiscogs artist) >>= \case
    Left err -> print err >> pure saved
    Right releases ->
      updateArtist (artistId artist) (artistDisplayTitle artist) saved releases configMiniflux

takeOldest :: Int -> Artists -> Artists
takeOldest n (Artists l) =
  Artists $
    HM.fromList $
      take n $
        sortBy (\(_, a) (_, b) -> compare (artistLastUpdate a) (artistLastUpdate b)) $
          HM.toList l

joinArtists :: Artists -> Artists -> Artists
joinArtists (Artists r) (Artists l) = Artists $ HM.unionWith (\l' r' -> l'{artistDisplayTitle = artistDisplayTitle r'}) l r

loadConfig :: FilePath -> ExceptT String IO (Config, Artists)
loadConfig dataFile = do
  configDir <-
    lift (lookupEnv "XDG_CONFIG_HOME")
      >>= maybe (fail "No XDG_CONFIG_HOME env variable set") pure

  config <-
    withExceptT
      show
      (ExceptT (Y.decodeFileEither (configDir </> "attulus" </> "config.yaml")))
  artists <-
    lift (doesFileExist dataFile)
      >>= bool (pure def) (withExceptT show $ ExceptT $ Y.decodeFileEither dataFile)
  pure (config, artists)

requestList :: DiscogsConfig -> ExceptT String IO Artists
requestList DiscogsConfig{..} = do
  initReq <- parseRequest "https://api.discogs.com"
  manager <- lift newTlsManager

  (_, artists) <- flip fix (1, HM.empty) \go (page, saved) -> do
    let request =
          initReq
            { path = "/lists/" <> pack discogsList
            , queryString = "token=" <> pack discogsKey <> "&page=" <> pack (show page)
            , requestHeaders = [("user-agent", userAgent)]
            }
    ListResponse _ pages (Artists artists) <- ExceptT $ withResponse request manager \response -> do
      throwErrorStatusCodes request response
      A.eitherDecode . fromStrict . fold <$> brConsume (responseBody response)
    if page < pages
      then go (succ page, HM.union artists saved)
      else pure (page, HM.union artists saved)
  return $ Artists artists

requestReleases :: DiscogsConfig -> Artist -> ExceptT String IO Releases
requestReleases DiscogsConfig{..} Artist{..} = do
  initReq <- parseRequest "https://api.discogs.com"
  manager <- lift newTlsManager

  (_, releases) <- flip fix (1, HM.empty) \go (page, saved) -> do
    let request =
          initReq
            { path = "/artists/" <> pack (show artistId) <> "/releases"
            , queryString = "token=" <> pack discogsKey <> "&page=" <> pack (show page)
            , requestHeaders = [("user-agent", userAgent)]
            }
    ArtistResponse _ pages (Releases releases) <- ExceptT $ withResponse request manager \response -> do
      throwErrorStatusCodes request response
      A.eitherDecode . fromStrict . fold <$> brConsume (responseBody response)
    if page < pages
      then go (succ page, HM.union releases saved)
      else pure (page, HM.union releases saved)
  return $ Releases releases

updateArtist :: Int -> String -> Artists -> Releases -> MinifluxConfig -> IO Artists
updateArtist artistId artistTitle artists (Releases releases) config = do
  let (Artist _ _ savedReleases _) = HM.lookupDefault def artistId $ unArtists artists
  newReleases <-
    Releases
      <$> foldM
        ( \hm Release{..} ->
            if HM.member releaseId hm || releaseType /= "master" || releaseRole /= "Main"
              then pure hm
              else updateRelease artistTitle config hm Release{..}
        )
        (unReleases savedReleases)
        releases
  now <- round <$> getPOSIXTime
  pure $
    Artists $
      HM.insert artistId (Artist artistId now newReleases artistTitle) (unArtists artists)

releaseToEntry :: String -> Release -> Entry
releaseToEntry author Release{..} =
  Entry
    { entryTitle = author <> ": " <> releaseTitle <> " (" <> show releaseYear <> ")"
    , entryUrl = "example.com"
    , entryAuthor = author
    , entryContent = ""
    , entryPublishedAt = 0
    , entryStatus = "unread"
    , entryStarred = False
    , entryTags = []
    , entryExternalId = kebabCase author <> "-" <> kebabCase releaseTitle
    , -- , entryCommentsUrl = "example.com"
      entryCommentsUrl = ""
    }

kebabCase :: String -> String
kebabCase = intercalate "-" . fmap (fmap toLower) . words

updateRelease
  :: String
  -> MinifluxConfig
  -> HM.HashMap Int Release
  -> Release
  -> IO (HM.HashMap Int Release)
updateRelease artistTitle MinifluxConfig{..} saved new@Release{..} = do
  initReq <- parseRequest "https://miniflux.my"
  let entry = releaseToEntry artistTitle new
      request =
        initReq
          { path = "/v1/feeds/" <> pack minifluxFeed <> "/entries/import"
          , method = "POST"
          , requestHeaders = [("x-auth-token", pack minifluxKey)]
          , requestBody = RequestBodyLBS $ A.encode entry
          }
  manager <-
    newTlsManagerWith
      (mkManagerSettings def{settingDisableCertificateValidation = True} Nothing)
  withResponse request manager \response -> do
    throwErrorStatusCodes request response
    pure $ HM.insert releaseId new saved
