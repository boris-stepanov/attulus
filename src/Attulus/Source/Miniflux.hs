{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Attulus.Source.Miniflux where

import Attulus.Source.Types

import Prelude

import GHC.Generics

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Text as T
import Data.Default

import Effectful
import Effectful.Fail
import Effectful.Concurrent
import Effectful.HttpClient
import Effectful.Log

import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

--------------------------------------- Types ----------------------------------

data MinifluxConfig = MinifluxConfig {minifluxKey :: String, minifluxFeed :: String}
  deriving (Generic, Show)

instance FromJSON MinifluxConfig where
  parseJSON = genericParseJSON opts

newtype EntryView = EntryView Entry deriving Generic

instance ToJSON EntryView where
  toJSON (EntryView entry) =
    let x = genericToJSON opts{fieldLabelModifier = underscoreFrom2} entry
        y = object ["published_at" .= (0 :: Int), "status" .= ("unread" :: String)]
     in case (x, y) of
          (Object a, Object b) -> Object (a <> b)
          _ -> error "Expected JSON objects"

--------------------------------------- IO -------------------------------------

pushEntry
  :: (Concurrent :> es, IOE :> es, Log :> es, Fail :> es)
  => MinifluxConfig -> Entry -> Eff es ()
pushEntry MinifluxConfig{..} entry = do
  runHttpClientTlsWith
    (mkManagerSettings def{settingDisableCertificateValidation = True} Nothing)
    do
      initReq <-
        parseRequest $
          "POST https://miniflux.my/v1/feeds/" <> minifluxFeed <> "/entries/import"
      let request =
            initReq
              { requestHeaders = [("x-auth-token", pack minifluxKey)]
              , requestBody = RequestBodyLBS (encode $ EntryView entry)
              }
      withResponseRetry 5 request \response ->
        let sci = statusCode (responseStatus response)
         in unless (sci < 300 && sci >= 200) do
            body <- liftIO (responseBody response)
            logAttention_ $ "Failed to push entry: " <> T.pack (unpack body)
