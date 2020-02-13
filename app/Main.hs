{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Api
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad.List (forM)
import Control.Monad.Except (withExceptT)
import Data.ByteString.Lazy.UTF8 (fromString)

server :: Server MainAPI
server = getJournals 
    :<|> getJournal

getJournals :: Handler [Journal]
getJournals = withServerErrors $ mapM journal [0 .. 10]
  where
    journal i = Journal <$> parseJournalId ('B' : 'C' : '-' : show i)
    withServerErrors = Handler . withExceptT (\s -> err500 {errBody = fromString s})

getJournal :: JournalId -> Handler Journal
getJournal j = return (Journal j)

app :: Application
app = serve mainApi server

main :: IO ()
main = do
  p <- fromMaybe "8080" <$> lookupEnv "PORT"
  run (read p) app
