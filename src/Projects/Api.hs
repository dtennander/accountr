module Projects.Api where

import Data.Aeson (FromJSON, ToJSON)
import Projects.Data

instance ToJSON ProjectId

instance FromJSON ProjectId