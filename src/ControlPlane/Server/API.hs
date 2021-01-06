{-# LANGUAGE DerivingVia #-}
module ControlPlane.Server.API where

import Colourista.IO                        (greenMessage)
import Network.Wai.Handler.Warp             (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Auth.Server ()

import ControlPlane.Environment
import ControlPlane.Server.API.Authentication
import ControlPlane.Server.API.Notification
import ControlPlane.Server.API.Types
import ControlPlane.Server.API.User
import ControlPlane.Server.API.Jobs
import ControlPlane.DB.User (User)

data ControlPlaneRoutes mode
  = ControlPlaneRoutes { notifications :: mode :- BasicAuth "" User :> "notifications" :> NotificationsRoutes
                       , users         :: mode :- BasicAuth "" User :> "users" :> UsersRoutes
                       , jobs          :: mode :- BasicAuth "" User :> "jobs" :> JobsRoutes
                       } deriving stock (Generic)

type API = ToServantApi ControlPlaneRoutes

app :: ControlPlaneEnv -> Application
app env = genericServeTWithContext
  (insertEnv env)
  routesHandlers
  (appContext env)
  where
    insertEnv :: ControlPlaneEnv -> ControlPlaneM IO a -> Handler a
    insertEnv s x = Handler . ExceptT . runControlPlaneM s $ x
    routesHandlers = ControlPlaneRoutes (\_ -> genericServerT notificationsRouteHandlers)
                                        (\_ -> genericServerT usersRoutesHandlers)
                                        (\_ -> genericServerT jobsRoutesHandlers)

appContext :: ControlPlaneEnv -> Context '[BasicAuthCheck User]
appContext env = loginCheck (pgPool env) :. EmptyContext

startService :: IO ()
startService = do
  env <- mkControlPlaneEnv
  greenMessage "[+] Starting control-plane-server on http://localhost:8008/"
  run 8008 (app env)

routeLayout :: IO ()
routeLayout = do
  env <- mkControlPlaneEnv
  putTextLn $ layoutWithContext (Proxy :: Proxy (ToServantApi ControlPlaneRoutes)) (appContext env)
  pure ()
