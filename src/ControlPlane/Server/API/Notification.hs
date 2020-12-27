module ControlPlane.Server.API.Notification 
  ( NotificationsRoutes
  , notificationsRouteHandlers
  , getAllNotifications
  , getNotificationById
  ) where

import Data.Time (getCurrentTime)
import Servant
import Servant.API.Generic
import Control.Monad.Except (MonadError (..))
import Servant.Server.Generic

import           ControlPlane.DB.Notification    (NewStatusPayload (..), Notification (..), NotificationId (..),
                                                  NotificationPayload (..), NotificationStatus (..),
                                                  NotificationStatusPayload (..))
import qualified ControlPlane.DB.Notification    as DB
import           ControlPlane.DB.Types           ()
import           ControlPlane.Environment        (ControlPlaneEnv (..))
import           ControlPlane.Model.Notification (mkNotification)
import           ControlPlane.Server.API.Helpers
import           ControlPlane.Server.API.Types

data NotificationsRoutes' mode
  = NotificationsRoutes' { notifications :: mode :- Get '[JSON] [Notification]
                         , notification  :: mode :- Capture "id" NotificationId
                                                 :> Get '[JSON] Notification
                         , postNotification :: mode :- ReqBody '[JSON] NotificationPayload
                                                    :> PostCreated '[JSON] NoContent
                         , setNotificationStatus :: mode :- Capture "id" NotificationId
                                                         :> "status" :> ReqBody '[JSON] NewStatusPayload
                                                         :> Post '[JSON] NoContent
                         } deriving (Generic)

type NotificationsRoutes = ToServantApi NotificationsRoutes'

notificationsRouteHandlers :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
                           => NotificationsRoutes' (AsServerT (ControlPlaneM m))
notificationsRouteHandlers =
  NotificationsRoutes' { notifications = getAllNotifications
                       , notification  = getNotificationById
                       , postNotification = postNotificationHandler
                       , setNotificationStatus = setNotificationStatusHandler
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

postNotificationHandler :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
                        => NotificationPayload -> ControlPlaneM m NoContent
postNotificationHandler payload = do
  pool <- asks pgPool
  notification <- mkNotification payload
  result <- liftIO $ DB.insertNotification pool notification
  case result of
    Left err -> internalServerError err
    Right _  -> pure NoContent

setNotificationStatusHandler :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
                             => NotificationId -> NewStatusPayload
                             -> ControlPlaneM m NoContent
setNotificationStatusHandler notificationId NewStatusPayload{status=statusPayload} = do
  pool <- asks pgPool
  ts   <- liftIO getCurrentTime
  let newStatus = case statusPayload of
        SetAsRead   -> NotificationRead ts
        SetAsUnread -> NotificationUnread
  notification <- getNotificationById notificationId
  let newNotification = notification{status = newStatus} :: Notification
  result <- liftIO $ DB.updateNotification pool newNotification
  case result of
    Left err -> internalServerError err
    Right _  -> pure NoContent
