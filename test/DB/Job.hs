module DB.Job (spec) where

import Database.PostgreSQL.Simple (Only (..))
import Relude.Unsafe              (read)
import Test.Hspec
import Test.Hspec.DB
import Data.Time
import Database.PostgreSQL.Transact

import ControlPlane.DB.Job
import DB.Helpers

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
      createdAt = read "2021-01-13 22:14:25 UTC" :: UTCTime
      runDate   = read "2021-01-13 23:14:25 UTC" :: UTCTime
      lockedAt  = Just $ read "2021-01-13 23:14:25 UTC" :: Maybe UTCTime
      attempts  = 0
   in Job{..}

spec :: Spec
spec = describeDB migrate "Job" $ do
  itDB "Insert job" $ do
    (Only expectedId) <- createJob job1 :: DBT IO (Only Int)
    let date = read "2021-01-13 21:17:25 UTC" :: UTCTime
    result <- getJobs date
    pure $ fmap jobId result `shouldBe` [Just expectedId]
  itDB "Get locked jobs" $ do
    (Only expectedId) <- createJob lockedJob :: DBT IO (Only Int)
    result <- getRunningJobs 
    pure $ fmap jobId result `shouldBe` [Just expectedId]
    
