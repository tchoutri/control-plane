module Web where

import Colourista.IO            (greenMessage)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Prelude                  hiding (get)
import Web.Scotty.Trans         (Options (Options), scottyOptsT)

import Environment (ControlPlaneEnv, mkEnv)
import Web.Router  (router)
import Web.Types   (WebM, runWebM)

startWebService :: IO ()
startWebService = do
  greenMessage "[+] Starting web server on http://localhost:8008"
  env <- mkEnv
  scottyOptsT serverOptions (runIO env) router

serverOptions :: Options
serverOptions = Options 0 settings
  where
    settings = setPort 8008 defaultSettings

runIO :: ControlPlaneEnv -> WebM a -> IO a
runIO env m = runWebM env m
