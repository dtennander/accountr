{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Error.Parser where

import Data.Aeson.Types (Parser, parserCatchError)

import Control.Monad.Error.Class (MonadError(..))

instance MonadError String Parser where
    throwError = fail
    catchError m handler = parserCatchError m f
      where f _ = handler