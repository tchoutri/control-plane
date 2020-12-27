module ControlPlane.Server.API.Helpers where

import Control.Monad.Except (MonadError)
import Colourista.IO
import Data.Aeson    (encode)
import Servant       (ServerError (..), err404, err500, throwError)

import ControlPlane.Server.API.Types

internalServerError :: (MonadIO m, MonadError ServerError (ControlPlaneM m))
                    => InternalError -> ControlPlaneM m a
internalServerError (ConstraintFailure msg) = do
  liftIO . redMessage $ "[!] " <> msg
  throwError $ err500 { errBody = encode $ ConstraintFailure msg }
internalServerError (NotificationNotFound nid) = do
  liftIO . redMessage $ "[!] " <> "Notification " <> show nid <> " not found"
  throwError $ err404 { errBody = encode $ NotificationNotFound nid }
internalServerError InsertionError = do
  liftIO . redMessage $ "[!]" <> "Could not insert incoming notification payload into database"
  throwError $ err500 { errBody = encode InsertionError }
