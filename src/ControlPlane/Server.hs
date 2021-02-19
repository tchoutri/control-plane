module ControlPlane.Server (startService) where

import ControlPlane.Web (startWebService)

startService :: IO ()
startService = startWebService
