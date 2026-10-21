{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Attulus.Source.Types where

import Prelude

import PackageInfo_attulus

import GHC.Generics

import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Char
import Data.Default
import qualified Data.HashMap.Lazy as HM
import Data.Version

import Control.Monad.Fix (fix)

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as T

import Effectful
import Effectful.Concurrent
import Effectful.Exception
import Effectful.Fail
import Effectful.HttpClient
import Effectful.Log

--------------------------------------- Types ----------------------------------

type LinkHandler c =
  forall es. (Concurrent :> es, IOE :> es, Log :> es, Fail :> es)
  => String
  -> ((String -> Maybe [Int] -> ([Entry], Maybe [Int])) -> Eff es ())
  -> c
  -> Eff es ()
newtype Artists = Artists {unArtists :: HM.HashMap String Artist} deriving Show

data Artist = Artist
  { artistId :: String
  , artistLastUpdate :: Int
  , artistNews :: HM.HashMap String [Int]
  , artistDisplayTitle :: String
  }
  deriving (Show, Generic)

data Entry = Entry
  { entryTitle :: String
  , entryUrl :: String
  , entryAuthor :: String
  , entryContent :: String
  , entryExternalId :: String
  }
  deriving (Show, Generic)

opts :: Options
opts =
  defaultOptions
    { fieldLabelModifier = underscoreFrom2
    , rejectUnknownFields = True
    , unwrapUnaryRecords = True
    }

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
  def = Artist def def HM.empty def

instance FromJSON Artists where
  parseJSON = withArray "artists" \arr -> do
    kvs <- traverse (fmap (\a -> (artistId a, a)) . parseJSON) arr
    pure $ Artists $ foldl' (\m (k, v) -> HM.insert k v m) HM.empty kvs

instance FromJSON Artist where
  parseJSON = withObject "artist" \w -> do
    artistId <- w .:? "id" .!= ""
    artistLastUpdate <- w .:? "last_update" .!= 0
    artistNews <- w .:? "news" .!= HM.empty
    artistDisplayTitle <- w .:? "display_title" .!= ""
    pure Artist{..}

instance ToJSON Artists where
  toJSON (Artists hm) = toJSON (HM.elems hm)

instance ToJSON Artist where
  toJSON Artist{..} =
    object
      [ "display_title" .= artistDisplayTitle
      , "id" .= artistId
      , "last_update" .= artistLastUpdate
      , "news" .= artistNews
      ]

--------------------------------------- Utils ----------------------------------

userAgent :: String
userAgent = capitalize name <> "RssBot/" <> showVersion version <> " +" <> homepage
 where
  capitalize "" = ""
  capitalize (x : xs) = toUpper x : xs

parseBody
  :: (IOE :> es, Fail :> es, FromJSON a) => FromJSON a => Response (IO ByteString) -> Eff es a
parseBody response = do
  body <- flip fix "" \go ret ->
    liftIO (responseBody response) >>= \case
      "" -> return ret
      some -> go (ret <> fromStrict some)
  either (fail . show) pure . A.eitherDecode $ body

sleep :: Concurrent :> es => Int -> Eff es ()
sleep = threadDelay . (* 1_000_000)

withResponseRetry
  :: forall (es :: [Effect]) a
   . (Concurrent :> es, Fail :> es, IOE :> es, HttpClient :> es, Log :> es)
  => Int -> Request -> (Response (IO ByteString) -> Eff es a) -> Eff es a
withResponseRetry n req run = go ConnectionClosed n
 where
  go content (-1) = logAttention_ (T.show content) >> fail (show content)
  go _ m =
    try (withResponse req run) >>= \case
      Right resp -> pure resp
      Left (InvalidUrlException _ reason) -> logAttention_ (T.pack reason) >> fail (show reason)
      Left (HttpExceptionRequest _ content) -> sleep 1 >> go content (m - 1)
