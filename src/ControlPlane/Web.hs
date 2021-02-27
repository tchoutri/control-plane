module ControlPlane.Web where

import Colourista.IO            (greenMessage)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Prelude                  hiding (get)
import Web.Scotty.Trans         (Options (Options), scottyOptsT)

import ControlPlane.Environment (ControlPlaneEnv, mkControlPlaneEnv)
import ControlPlane.Web.Router  (router)
import ControlPlane.Web.Types   (ControlPlaneM, runControlPlaneM)

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
