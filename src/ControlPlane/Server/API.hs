{-# LANGUAGE DerivingVia #-}
module ControlPlane.Server.API where

import Data.UUID
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import ControlPlane.Environment
import ControlPlane.Model.Notification

data NotificationsRoute mode
  = NotificationsRoute { notifications :: mode :- "notifications" :> Get '[JSON] [Notification]
                       , notification  :: mode :- "notification" :> Capture "id" NotificationId :> Get '[JSON] Notification
                       } deriving (Generic)

-- newtype ControlPlaneM a b = ControlPlaneM (R ControlPlaneEnv)
type ControlPlaneM = ReaderT ControlPlaneEnv Handler

notificationsRoute :: NotificationsRoute (AsServerT ControlPlaneM)
notificationsRoute = 
  NotificationsRoute { notifications = getNotifications
                     , notification  = getNotificationById
                     }

api :: Proxy (ToServantApi NotificationsRoute)
api = genericApi (Proxy :: Proxy NotificationsRoute)

app :: ControlPlaneEnv -> Application
app env = genericServeT (insertEnv env) notificationsRoute
  where
    insertEnv :: ControlPlaneEnv -> ControlPlaneM a -> Handler a
    insertEnv s x = runReaderT x s

startService :: IO ()
startService = do
  env <- mkControlPlaneEnv
  putTextLn "Starting control-plane-server on http://localhost:8008/"
  run 8008 (app env)

routeLayout :: Text
routeLayout = layout (Proxy :: Proxy (ToServantApi NotificationsRoute))

getNotifications :: ControlPlaneM [Notification]
getNotifications = pure [n1]

getNotificationById :: NotificationId -> ControlPlaneM Notification
getNotificationById _id = pure n1

n1 :: Notification
n1 = Notification { notificationId = nid
                  , device     = "Téléphone"
                  , title      = "Notification!"
                  , message    = "Bouh!"
                  , status     = NotificationUnread
                  , receivedAt = time
                  }
  where
    nid = NotificationId nil
    time = fromJust . readMaybe $ "2020-12-24 10:26:34.815690388 UTC"
