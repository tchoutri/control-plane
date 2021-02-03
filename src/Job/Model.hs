module Job.Model where

import Data.Aeson (FromJSON)
import Data.Time (UTCTime, getCurrentTime)

import Job.DB (Job (..), Payload)

data JobInfo
  = JobInfo { newPayload :: Payload
            , newRunDate :: UTCTime
            }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

mkJob :: JobInfo -> IO Job
mkJob JobInfo{..} = do
  createdAt <- getCurrentTime
  let jobId = Nothing
  let lockedAt = Nothing
  let attempts = 0
  pure Job{jobId, lockedAt, attempts, payload=newPayload, runDate=newRunDate, createdAt}
