{-# LANGUAGE OverloadedLists #-}
module DB.JobSpec (spec) where

import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Transact (DBT)
import Relude.Unsafe (fromJust, read)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldReturn)

import DB.Helpers (migrate)
import Job.DB (Job (..), Payload (CheckWebsite), Website (Website), createJob, getJobs, getRunningJobs, unlockJob)

job1 :: Job
job1 =
  let jobId     = Nothing
      payload   = CheckWebsite (Website "https://theophile.choutri.eu")
      createdAt = read "2021-01-13 21:14:25 UTC" :: UTCTime
      runDate   = read "2021-01-13 22:14:25 UTC" :: UTCTime
      lockedAt  = Nothing
      attempts  = 0
   in Job{..}

lockedJob :: Job
lockedJob =
  let jobId     = Nothing
      payload   = CheckWebsite (Website "https://theophile.choutri.eu")
      createdAt = read "2021-01-13 23:14:25 UTC" :: UTCTime
      runDate   = read "2021-01-13 23:15:25 UTC" :: UTCTime
      lockedAt  = Just $ read "2021-01-13 23:14:25 UTC" :: Maybe UTCTime
      attempts  = 0
   in Job{..}

spec :: Spec
spec = describeDB migrate "Job" $ do
  itDB "Insert job" $ do
    (Only expectedId) <- createJob job1 :: DBT IO (Only Int)
    let date = read "2021-01-13 22:17:25 UTC" :: UTCTime
    result <- getJobs date
    fmap jobId result `shouldBe` [Just expectedId]
  itDB "Get locked jobs" $ do
    (Only expectedId) <- createJob lockedJob :: DBT IO (Only Int)
    result <- getRunningJobs
    fmap jobId result `shouldBe` [Just expectedId]
  itDB "Unlock job" $ do
    result <- getRunningJobs
    mapM_ (\j -> unlockJob $ fromJust $ jobId j) result
    getRunningJobs `shouldReturn` []
