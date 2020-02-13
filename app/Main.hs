{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Api
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

server :: Server MainAPI
server = getJournal

getJournal :: JournalId -> Handler Journal
getJournal j = return (Journal j)

app :: Application
app = serve mainApi server

main :: IO ()
main = do
  p <- fromMaybe "8080" <$> lookupEnv "PORT"
  run (read p) app
