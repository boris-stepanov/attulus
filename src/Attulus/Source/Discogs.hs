{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Attulus.Source.Discogs where

import Attulus.Source.Types

import Prelude

import GHC.Generics

import Data.Aeson.Types
import Data.Bool (bool)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char
import Data.Default
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List (intercalate, sortBy)
import Data.Maybe
import qualified Data.Text as T

import Control.Monad.Fix (fix)

import Effectful
import Effectful.Concurrent
import Effectful.Fail
import Effectful.HttpClient
import Effectful.Log

import Network.HTTP.Client (host, path, queryString)
import Network.HTTP.Client.TLS

--------------------------------------- Types ----------------------------------

data DiscogsConfig = DiscogsConfig {discogsKey :: String, discogsList :: String}
  deriving (Generic, Show)

instance FromJSON DiscogsConfig where
  parseJSON = genericParseJSON opts

data ArtistResponse = ArtistResponse Int Int Releases
newtype Releases = Releases (HM.HashMap Int Release)

data Release = Release
  { releaseId :: Int
  , releaseType :: String
  , releaseRole :: String
  , releaseTitle :: String
  , releaseYear :: Int
  , releaseArtist :: String
  }
  deriving (Generic, Show, Eq)

instance Default Release where
  def = Release 0 "" "" "" 0 ""

instance Ord Release where
  compare a b = compare (releaseYear a) (releaseYear b)

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

instance FromJSON Releases where
  parseJSON =
    fmap (Releases . foldl (\hm r@Release{..} -> HM.insert releaseId r hm) HM.empty)
      . parseJSONList

instance FromJSON ArtistResponse where
  parseJSON = withObject "artist_response" \w -> do
    Releases releases <- w .: "releases"
    (page, pages) <-
      w .:? "pagination" .!= object ["page" .= (1 :: Int), "pages" .= (1 :: Int)] >>= withObject "pagination" \v -> do
        (,) <$> v .: "page" <*> v .: "pages"
    pure $
      ArtistResponse
        page
        pages
        ( Releases $
            HM.filter (\Release{..} -> releaseType == "master" && releaseRole == "Main") releases
        )

--------------------------------------- IO -------------------------------------

handleDiscogs :: LinkHandler DiscogsConfig
handleDiscogs url applyNews config = do
  Releases releases <- requestReleases url config
  applyNews (getNew releases)
  logInfo_ ("Sent " <> T.show (HM.size releases) <> " entries")
 where
  getNew :: HM.HashMap Int Release -> String -> Maybe [Int] -> ([Entry], Maybe [Int])
  getNew releases artist old' =
    let old = HS.fromList (fromMaybe mempty old')
        sorted = sortBy (\(_, rx) (_, ry) -> compare rx ry) $ HM.toList releases
        (entries, new) =
          foldMap
            (\(id', r) -> (bool [releaseToEntry artist r] [] (HS.member id' old), HS.singleton id'))
            sorted
     in (entries, Just (HS.toList (old <> new)))

requestReleases
  :: (Concurrent :> es, IOE :> es, Log :> es, Fail :> es)
  => String -> DiscogsConfig -> Eff es Releases
requestReleases url DiscogsConfig{..} = runHttpClientWith tlsManagerSettings do
  let artistId = reverse $ takeWhile ('/' /=) $ reverse url
  initReq <-
    parseRequest $ "GET https://api.discogs.com/artists/" <> artistId <> "/releases"

  (_, releases) <- flip fix (1, HM.empty) \go (page, saved) -> do
    logInfo_ $ "Requesting " <> T.pack (unpack $ host initReq <> path initReq)
    let request =
          initReq
            { queryString = "token=" <> pack discogsKey <> "&page=" <> pack (show page)
            , host = "api.discogs.com"
            , requestHeaders = [("user-agent", pack userAgent)]
            }
    ArtistResponse _ pages (Releases releases) <- withResponseRetry 5 request parseBody

    if page < pages
      then go (succ page, HM.union releases saved)
      else pure (page, HM.union releases saved)
  return $ Releases releases

releaseToEntry :: String -> Release -> Entry
releaseToEntry author Release{..} =
  Entry
    { entryTitle = author <> ": " <> releaseTitle <> " (" <> show releaseYear <> ")"
    , entryUrl = "example.com"
    , entryAuthor = author
    , entryContent = ""
    , entryExternalId = kebabCase author <> "-" <> kebabCase releaseTitle
    }

kebabCase :: String -> String
kebabCase = intercalate "-" . fmap (fmap toLower) . words
