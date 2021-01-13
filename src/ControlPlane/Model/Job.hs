module ControlPlane.Model.Job where

import Data.Time (getCurrentTime, UTCTime)
import Data.Aeson (FromJSON)

import ControlPlane.DB.Job (Job (..), Payload)

data JobInfo
  = JobInfo { newPayload :: Payload
            , newRunDate :: UTCTime
            } deriving stock (Eq, Show, Generic)
              deriving anyclass (FromJSON)

mkJob :: JobInfo -> IO Job
mkJob JobInfo{..} = do
  createdAt <- getCurrentTime
  let jobId = Nothing
  let lockedAt = Nothing
  let attempts = 0
  pure Job{jobId, lockedAt, attempts, payload=newPayload, runDate=newRunDate, createdAt}
