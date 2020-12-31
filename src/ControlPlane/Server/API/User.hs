module ControlPlane.Server.API.User where

import           Control.Monad.Except            (MonadError (..))
import           Data.Time
import           Data.UUID.V4
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           ControlPlane.DB.User            (NewUser (..), User (..), UserId (..), UserInfo (..))
import qualified ControlPlane.DB.User            as DB
import           ControlPlane.Environment        (ControlPlaneEnv (..))
import           ControlPlane.Model.User
import           ControlPlane.Server.API.Helpers (internalServerError)
import           ControlPlane.Server.API.Types   (ControlPlaneM)

import Database.PostgreSQL.Simple (Only (..))

data UsersRoutes' mode
  = UsersRoutes' { getUser :: mode :- Capture "id" UserId :> Get '[JSON] UserInfo
                 , newUser :: mode :- ReqBody '[JSON] NewUser :> PostCreated '[JSON] UserInfo
                 } deriving stock (Generic)

type UsersRoutes = ToServantApi UsersRoutes' 

usersRoutesHandlers :: (MonadIO m, MonadError ServerError (ControlPlaneM m))
                    => UsersRoutes' (AsServerT (ControlPlaneM m))
usersRoutesHandlers =
  UsersRoutes' { getUser = getUserHandler
               , newUser = newUserHandler
               }

getUserHandler :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
               => UserId -> ControlPlaneM m UserInfo
getUserHandler userId = do
  pool <- asks pgPool
  result <- liftIO $ DB.getUserById pool userId
  case result of
    Left err          -> internalServerError err
    Right (Only user) -> pure $ toUserInfo user

newUserHandler :: (MonadIO m, (MonadError ServerError (ControlPlaneM m)))
               => NewUser -> ControlPlaneM m UserInfo
newUserHandler NewUser{..} = do
  pool <- asks pgPool
  hashedPassword <- DB.hashPassword password
  timestamp      <- liftIO getCurrentTime
  userId         <- liftIO $ UserId <$> nextRandom
  let user = User{userId=userId, username=username, displayName=displayName, password=hashedPassword, createdAt=timestamp, updatedAt=timestamp}
  result <- liftIO $ DB.insertUser pool user
  case result of
    Left err -> internalServerError err
    Right _  -> getUserHandler userId
