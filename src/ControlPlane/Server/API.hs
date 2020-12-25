{-# LANGUAGE DerivingVia #-}
module ControlPlane.Server.API where

import Colourista.IO            (greenMessage, redMessage)
import Data.Maybe               (fromJust)
import Data.UUID
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Data.Aeson

import ControlPlane.Environment
import ControlPlane.DB.Types as DB
import ControlPlane.DB.Notification as DB
import ControlPlane.Server.API.Types

data NotificationsRoute mode
  = NotificationsRoute { notifications :: mode :- "notifications" :> Get '[JSON] [Notification]
                       , notification  :: mode :- "notification" :> Capture "id" NotificationId
                                                                 :> Get '[JSON] Notification
                       } deriving (Generic)

type ControlPlaneM = ReaderT ControlPlaneEnv Handler

notificationsRoute :: NotificationsRoute (AsServerT ControlPlaneM)
notificationsRoute = 
  NotificationsRoute { notifications = getAllNotifications
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
  greenMessage "[+] Starting control-plane-server on http://localhost:8008/"
  run 8008 (app env)

routeLayout :: Text
routeLayout = layout (Proxy :: Proxy (ToServantApi NotificationsRoute))

getAllNotifications :: ControlPlaneM [Notification]
getAllNotifications = do
  pool <- asks pgPool
  result <- liftIO $ DB.getNotifications pool 
  case result of
    Left err            -> incoherentDataError err
    Right notifications -> pure notifications

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

incoherentDataError :: IncoherentDataException -> ControlPlaneM a
incoherentDataError (DB.IDE msg) = do
  liftIO . redMessage $ "[!] " <> msg
  lift . throwError $ err501 { errBody = encode $ ConstraintFailure msg }
