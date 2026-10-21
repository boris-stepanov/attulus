module Attulus.Source (
  Links (..),
  Link (..),
  Artists (..),
  Artist (..),
  Config (..),
  AttulusConfig (..),
  MinifluxConfig (..),
  TicketMasterConfig (..),
  Entry (..),
  LinkHandler,
  requestList,
  requestArtist,
  pushEntry,
  handleDiscogs,
) where

import Attulus.Source.Discogs
import Attulus.Source.Miniflux
import Attulus.Source.Musicbrainz
import Attulus.Source.Types

import Data.Aeson.Types
import Effectful.Log
import GHC.Generics

--------------------------------------- Types ----------------------------------

data Config = Config
  { configMusicbrainz :: MusicbrainzConfig
  , configMiniflux :: MinifluxConfig
  , configDiscogs :: DiscogsConfig
  , configAttulus :: AttulusConfig
  , configTicketMaster :: TicketMasterConfig
  }
  deriving (Generic, Show)

newtype AttulusConfig = AttulusConfig {attulusLogLevel :: LogLevel}
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts

data TicketMasterConfig = TicketMasterConfig deriving Show

instance FromJSON TicketMasterConfig where
  parseJSON = const $ pure TicketMasterConfig

instance FromJSON AttulusConfig where
  parseJSON = genericParseJSON opts{unwrapUnaryRecords = False}
