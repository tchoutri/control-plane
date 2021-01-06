module ControlPlane.Server.API.Jobs where

import           Control.Monad.Except          (MonadError (..))
import           Data.Time
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           ControlPlane.DB.Helpers         (runDB)
import           ControlPlane.DB.Job             (Job)
import qualified ControlPlane.DB.Job             as DB
import           ControlPlane.Environment        (ControlPlaneEnv (..))
import           ControlPlane.Model.Job
import           ControlPlane.Server.API.Helpers (internalServerError)
import           ControlPlane.Server.API.Types

data JobsRoutes' mode
  = JobsRoutes' { getJobs :: mode :- Get '[JSON] [Job]
                , createJob :: mode :- "create" :> ReqBody '[JSON] JobInfo :> PostCreated '[JSON] NoContent
                } deriving stock (Generic)

type JobsRoutes = ToServantApi JobsRoutes'

jobsRoutesHandlers :: (MonadIO m, MonadError ServerError (ControlPlaneM m))
                   => JobsRoutes' (AsServerT (ControlPlaneM m))
jobsRoutesHandlers = 
  JobsRoutes' { getJobs = getJobsHandler
              , createJob = createJobHandler
              }

getJobsHandler :: (MonadIO m, MonadError ServerError (ControlPlaneM m))
               => ControlPlaneM m [Job]
getJobsHandler = do
  pool <- asks pgPool
  currentDate <- liftIO getCurrentTime
  result <- liftIO $ runDB pool $ DB.getJobs currentDate
  case result of
    Left err   -> internalServerError err
    Right jobs -> pure jobs

createJobHandler :: (MonadIO m, MonadError ServerError (ControlPlaneM m))
               => JobInfo -> ControlPlaneM m NoContent
createJobHandler jobPayload = do
  pool <- asks pgPool
  job <- liftIO $ mkJob jobPayload
  result <- liftIO $ runDB pool $ DB.createJob job
  case result of
    Left err -> internalServerError err
    Right _  -> pure NoContent
