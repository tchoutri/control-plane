module ControlPlane.Server.API.Jobs.CheckWebsite where

import Data.Aeson

newtype Website = Website { url :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
