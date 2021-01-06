module ControlPlane.Model.Job where

import Data.Time (getCurrentTime, UTCTime)
import Data.Aeson (FromJSON)

import ControlPlane.DB.Job (Job (..), Payload)

data JobInfo
  = JobInfo { payload :: Payload
            , runDate :: UTCTime
            } deriving stock (Eq, Show, Generic)
              deriving anyclass (FromJSON)

mkJob :: JobInfo -> IO Job
mkJob JobInfo{..} = do
  createdAt <- getCurrentTime
  let jobId = Nothing
  pure Job{..}
