{-# LANGUAGE DerivingVia #-}
module ControlPlane.Server.API where

import Colourista.IO (greenMessage)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import           ControlPlane.Environment
import           ControlPlane.Server.API.Notification
import           ControlPlane.Server.API.Types

newtype ControlPlaneRoutes mode
  = ControlPlaneRoutes { notifications :: mode :- "notifications" :> NotificationsRoutes }
    deriving stock (Generic)

type API = ToServantApi ControlPlaneRoutes

app :: ControlPlaneEnv -> Application
app env = genericServeT
  (insertEnv env)
  routesHandlers
  where
    insertEnv :: ControlPlaneEnv -> ControlPlaneM IO a -> Handler a
    insertEnv s x = Handler . ExceptT . runControlPlaneM s $ x
    routesHandlers = ControlPlaneRoutes (genericServerT notificationsRouteHandlers)

startService :: IO ()
startService = do
  env <- mkControlPlaneEnv
  greenMessage "[+] Starting control-plane-server on http://localhost:8008/"
  run 8008 (app env)

routeLayout :: Text
routeLayout = layout (Proxy :: Proxy (ToServantApi ControlPlaneRoutes))
