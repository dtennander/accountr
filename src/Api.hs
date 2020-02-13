{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Data.Aeson.Types as T
import Servant.API
import Control.Monad.Except (withExceptT)
import Data.Char (isUpper)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson (toJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (pack, unpack)
import GHC.Exception.Type (Exception)
import Data.Proxy
import Data.Functor
import Control.Monad.Error.Class
import Control.Arrow (left)

type MainAPI = "journal" :> Capture "journalId" JournalId :> Get '[JSON] Journal
          :<|> "journal" :> Capture "journalId" JournalId :> ReqBody '[JSON] Journal :> Post '[JSON] Journal

mainApi :: Proxy MainAPI
mainApi = Proxy

newtype Journal = Journal {
  journalId :: JournalId
} deriving (Eq, Show, Generic)

instance ToJSON Journal

instance FromJSON Journal

data JournalId = JournalId Project Int deriving (Eq, Show)

instance ToJSON JournalId where
  toJSON (JournalId (Project (a,b)) i) =  T.String . pack $ (a : b :'-' : show i)

instance FromJSON JournalId where
  parseJSON (T.String t) = parseJournalId . unpack $ t

instance FromHttpApiData JournalId where
  parseUrlPiece = left pack . parseJournalId . unpack

parseJournalId :: MonadError String m => String -> m JournalId
parseJournalId (a:b:'-': i) = project [a,b] <&> (\p -> JournalId p (read i))
parseJournalId _ = throwError "JournalId does not follow form XX-NNN"

newtype Project = Project (Char, Char) deriving (Eq, Show, Generic)

instance ToJSON Project

instance FromJSON Project

project :: MonadError String m => String -> m Project
project [a, b] | isUpper a && isUpper b = return (Project (a, b))
project s = throwError ('"' : s ++ "\" Could not be converted to a project!")

instance MonadError String Parser where
    throwError = fail
    catchError m handler = parserCatchError m f
      where f _ = handler
