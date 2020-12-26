module ControlPlane.Server.API.Notification 
  ( NotificationsRoutes
  , notificationsRouteHandlers
  , getAllNotifications
  , getNotificationById
  ) where

import Servant
import Servant.API.Generic
import Control.Monad.Except (MonadError (..))
import Servant.Server.Generic

import           ControlPlane.DB.Notification    (Notification (..), NotificationId (..))
import qualified ControlPlane.DB.Notification    as DB
import           ControlPlane.DB.Types           ()
import           ControlPlane.Environment        (ControlPlaneEnv (..))
import           ControlPlane.Server.API.Helpers
import           ControlPlane.Server.API.Types

data NotificationsRoutes' mode
  = NotificationsRoutes' { notifications :: mode :- Get '[JSON] [Notification]
                         , notification  :: mode :- Capture "id" NotificationId :> Get '[JSON] Notification
                         } deriving (Generic)

type NotificationsRoutes = ToServantApi NotificationsRoutes'

notificationsRouteHandlers :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
                           => NotificationsRoutes' (AsServerT (ControlPlaneM m))
notificationsRouteHandlers =
  NotificationsRoutes' { notifications = getAllNotifications
                       , notification  = getNotificationById
                       }

getAllNotifications :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
                    => ControlPlaneM m [Notification]
getAllNotifications = do
  pool <- asks pgPool
  result <- liftIO $ DB.getNotifications pool
  case result of
    Left err            -> internalServerError err
    Right notifications -> pure notifications

getNotificationById :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
                    => NotificationId -> ControlPlaneM m Notification
getNotificationById notificationId = do
  pool <- asks pgPool
  result <- liftIO $ DB.getNotificationById pool notificationId
  case result of
    Left err  -> internalServerError err
    Right [n] -> pure n
    Right _   -> internalServerError (NotificationNotFound (getNotificationId notificationId))