module Api (
  MainAPI,
  mainApi,
  module Journals.Api
) where

import Journals.Api
import Data.Proxy (Proxy(..))

type MainAPI = JournalApi
mainApi :: Proxy MainAPI
mainApi = Proxy

