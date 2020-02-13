{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Api

server :: Server MainAPI
server = getJournal

getJournal :: JournalId -> Handler Journal
getJournal j = return (Journal j)

app :: Application
app = serve mainApi server

main :: IO ()
main = run 8081 app
