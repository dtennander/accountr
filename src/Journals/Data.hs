{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Journals.Data (
  Journal(..),
  JournalId,
  parseJournalId
) where

import GHC.Generics (Generic)

import Control.Monad.Except (MonadError, throwError)
import Projects.Data (ProjectId, projectId)
import Data.Functor ((<&>))

newtype Journal = Journal {
  journalId :: JournalId
} deriving (Eq, Show, Generic)

data JournalId = JournalId ProjectId Int deriving (Eq)

instance Show JournalId where
  show (JournalId p i) = show p ++ "-" ++ show i

parseJournalId :: MonadError String m => String -> m JournalId
parseJournalId (a:b:'-': i) = projectId [a,b] <&> (\p -> JournalId p (read i))
parseJournalId _ = throwError "JournalId does not follow form XX-NNN"