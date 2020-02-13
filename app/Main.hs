{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant.API
import Control.Monad.Except (withExceptT, ExceptT, liftIO)
import Servant
import Network.Wai.Handler.Warp
import Data.Char (isUpper)
import Servant.Server.Internal.Handler
import Control.Monad.Error.Class (liftEither, MonadError)
import Data.ByteString.Lazy.UTF8
import Data.Aeson
import Data.Aeson.Types as T
import GHC.Generics
import Data.Text
import Api
import Control.Exception (Exception, displayException)

server :: Server MainAPI
server = getJournal :<|> createJournal

getJournal :: JournalId -> Handler Journal
getJournal j = return (Journal j)

createJournal :: JournalId -> Journal -> Handler Journal
createJournal id = return

liftStringEither = Handler . withExceptT s2e . liftEither
  where
    s2e e = err500 { errBody = fromString e }

app :: Application
app = serve mainApi server

main :: IO ()
main = run 8081 app
