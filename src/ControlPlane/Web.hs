module ControlPlane.Web where

import Colourista.IO
import Database.PostgreSQL.Simple (Connection)
import Prelude                    hiding (get)
import Web.Spock
import Web.Spock.Config

import ControlPlane.Environment
import ControlPlane.Web.Router
import ControlPlane.Web.Types

startWebService :: IO ()
startWebService = do
  greenMessage "[+] Starting web server"
  s <- newEmptyMVar
  env <- mkControlPlaneEnv
  config <- defaultSpockCfg EmptySession (PCPool (pgPool env)) (AppState s)
  runSpock 8008 (spock config router)

handleHello :: Text -> ActionCtxT () (WebStateM Connection MySession MyAppState) b
handleHello name = do
  (AppState ref) <- getState
  visitorNumber <- liftIO $ do
    value <- takeMVar ref
    let newValue = value + 1
    putMVar ref newValue
    pure newValue
  html $ "<h1> Hello " <> name <> "! </h1>\n<p>You are visitor number " <> show visitorNumber
