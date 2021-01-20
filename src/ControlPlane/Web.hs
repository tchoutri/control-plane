module ControlPlane.Web where

import Colourista.IO
import Network.Wai.Handler.Warp
import Prelude                  hiding (get)
import Web.Scotty.Trans

import ControlPlane.Environment
import ControlPlane.Web.Router
import ControlPlane.Web.Types

startWebService :: IO ()
startWebService = do
  greenMessage "[+] Starting web server on http://localhost:8008"
  env <- mkControlPlaneEnv
  scottyOptsT serverOptions (runIO env) router

serverOptions :: Options
serverOptions = Options 0 settings
  where
    settings = setPort 8008 defaultSettings

runIO :: ControlPlaneEnv -> ControlPlaneM a -> IO a
runIO env m = runControlPlaneM env m
