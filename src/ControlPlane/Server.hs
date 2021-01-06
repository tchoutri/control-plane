module ControlPlane.Server where

import qualified ControlPlane.Server.API as API

startService :: IO ()
startService = API.startService
