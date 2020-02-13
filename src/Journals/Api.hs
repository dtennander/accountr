{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Journals.Api (
  JournalApi,
  module Journals.Data
) where

import Data.Aeson.Types as T
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (pack, unpack)
import Control.Arrow (left)
import Servant.API (FromHttpApiData(..), Capture, Get, JSON, (:>), (:<|>))
import Journals.Data
import Control.Monad.Error.Parser

type JournalApi = "journals" :> Get '[JSON] [Journal]
             :<|> "journals" :> Capture "journalId" JournalId :> Get '[JSON] Journal

instance ToJSON Journal

instance FromJSON Journal

instance ToJSON JournalId where
  toJSON =  T.String . pack . show

instance FromJSON JournalId where
  parseJSON (T.String t) = parseJournalId . unpack $ t

instance FromHttpApiData JournalId where
  parseUrlPiece = left pack . parseJournalId . unpack