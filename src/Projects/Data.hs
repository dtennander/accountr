{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Projects.Data (
  ProjectId,
  projectId
) where

import GHC.Generics (Generic)
import GHC.Unicode (isUpper)
import Control.Monad.Except (MonadError, throwError)

newtype ProjectId = ProjectId (Char, Char) deriving (Eq, Generic)

instance Show ProjectId where
  show (ProjectId (a, b)) = [a, b]

projectId :: MonadError String m => String -> m ProjectId
projectId [a, b] | isUpper a && isUpper b = return (ProjectId (a, b))
projectId s = throwError ('"' : s ++ "\" Could not be converted to a project!")