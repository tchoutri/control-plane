module ControlPlane.Server where

import ControlPlane.Web (startWebService)

startService :: IO ()
startService = startWebService
