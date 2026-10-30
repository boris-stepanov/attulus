{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Attulus.Source.Musicbrainz where

import Prelude

import Attulus.Source.Types

import GHC.Generics

import Data.Aeson
import Data.List (intercalate)
import Data.String (fromString)

import qualified Data.HashMap.Lazy as HM
import Effectful
import Effectful.Fail
import Effectful.Concurrent
import Effectful.Log
import Effectful.HttpClient
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Local (State, get)

import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.IORef
import System.IO.Unsafe

--------------------------------------- Types ----------------------------------

data MusicbrainzConfig = MusicbrainzConfig
  { musicbrainzClientId :: String
  , musicbrainzClientSecret :: String
  , musicbrainzRefreshToken :: String
  , musicbrainzCollection :: String
  }
  deriving (Generic, Show)

instance FromJSON MusicbrainzConfig where
  parseJSON = genericParseJSON opts

newtype ArtistsView = ArtistsView Artists

instance FromJSON ArtistsView where
  parseJSON = withObject "collection" \w -> do
    kvs <-
      w .: "artists"
        >>= withArray "artists" (traverse (fmap (\(ArtistView a) -> (artistId a, a)) . parseJSON))
    pure $ ArtistsView $ Artists $ foldl' (\l (k, v) -> HM.insert k v l) HM.empty kvs

newtype ArtistView = ArtistView Artist

instance FromJSON ArtistView where
  parseJSON = withObject "artist" \v -> do
    artistId <- v .: "id"
    artistDisplayTitle <- v .: "name"
    let artistLastUpdate = 0
        artistNews = HM.empty
    pure $ ArtistView $ Artist{..}

newtype AuthResponse = AuthResponse String

instance FromJSON AuthResponse where
  parseJSON = withObject "auth" \v -> AuthResponse <$> v .: "access_token"

newtype Links = Links {unLinks :: [Link]} deriving Show

instance FromJSON Links where
  parseJSON = withObject "relations" \v -> Links <$> v .: "relations"

data Link = Link {linkType :: String, linkUrl :: String} deriving Show

instance FromJSON Link where
  parseJSON = withObject "relation" \v -> Link <$> v .: "type" <*> (v .: "url" >>= \w -> w .: "resource")

--------------------------------------- API ------------------------------------

authToken :: IORef String
authToken = unsafePerformIO (newIORef "")
{-# NOINLINE authToken #-}

requestList :: (Concurrent :> es, IOE :> es, Log :> es, Fail :> es, Reader MusicbrainzConfig :> es) => Eff es Artists
requestList = runHttpClientWith tlsManagerSettings do
  MusicbrainzConfig{..} <- ask
  authRequest' <- parseRequest "https://musicbrainz.org/oauth2/token"
  let authRequest =
        authRequest'
          { method = "POST"
          , requestBody =
              RequestBodyLBS $
                fromString $
                  intercalate
                    "&"
                    [ "grant_type=refresh_token"
                    , "refresh_token=" <> musicbrainzRefreshToken
                    , "client_id=" <> musicbrainzClientId
                    , "client_secret=" <> musicbrainzClientSecret
                    ]
          , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
          }
  logTrace_ "Requesting token"
  AuthResponse token <- withResponseRetry 5 authRequest parseBody
  liftIO $ writeIORef authToken token

  collectionRequest' <-
    parseRequest $ "https://musicbrainz.org/ws/2/artist?limit=500&collection=" <> musicbrainzCollection
  let collectionRequest =
        collectionRequest'
          { requestHeaders =
              [ ("Accept", "application/json")
              , ("User-Agent", fromString userAgent)
              , ("Authorization", "Bearer " <> fromString token)
              ]
          }

  sleep 2

  ArtistsView artists <- withResponseRetry 5 collectionRequest parseBody
  return artists

requestArtist :: (Concurrent :> es, IOE :> es, Log :> es, Fail :> es, State Artist :> es) => Eff es Links
requestArtist = runHttpClientWith tlsManagerSettings do
  Artist{..} <- get
  token <- liftIO $ readIORef authToken
  artistRequest' <-
    parseRequest $ "https://musicbrainz.org/ws/2/artist/" <> artistId <> "?inc=url-rels"
  let artistRequest =
        artistRequest'
          { requestHeaders =
              [ ("Accept", "application/json")
              , ("User-Agent", fromString userAgent)
              , ("Authorization", "Bearer " <> fromString token)
              ]
          }

  sleep 2
  logTrace_ "Fetching references"
  withResponseRetry 5 artistRequest parseBody
