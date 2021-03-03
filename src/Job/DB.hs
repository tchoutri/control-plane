{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
module Job.DB where

import Data.Aeson
import Data.Time
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Transact (DBT)

data Job
  = Job { jobId     :: Maybe Int
        , payload   :: Payload
        , createdAt :: UTCTime
        , runDate   :: UTCTime
        , lockedAt  :: Maybe UTCTime
        , attempts  :: Integer
        } deriving stock (Eq, Show, Generic)
          deriving anyclass (FromJSON, ToJSON, FromRow)

instance Entity Job where
  tableName = "jobs"
  primaryKey = "job_id"
  fields = [ "job_id"
           , "payload"
           , "created_at"
           , "run_date"
           , "locked_at"
           , "attempts"
           ]

newtype Website = Website { url :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data Payload = CheckWebsite Website
             | GrabJSON Text
  deriving stock (Eq, Show, Generic)

instance ToJSON Payload where
  toJSON = genericToJSON payloadJSONOptions

instance FromJSON Payload where
  parseJSON = genericParseJSON payloadJSONOptions

payloadJSONOptions :: Options
payloadJSONOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName = "type", contentsFieldName = "data" }
  }

instance ToField Payload where
  toField = toJSONField

instance FromField Payload where
  fromField = fromJSONField

instance ToRow Job where
  toRow Job{..} = toRow (payload, createdAt, runDate, lockedAt, attempts)

getAllJobs :: DBT IO (Vector Job)
getAllJobs = query_ Select (_select @Job)

getJobs :: UTCTime -> DBT IO (Vector Job)
getJobs currentTime = query Select q (Only currentTime)
  where q = [sql| SELECT id, payload, created_at, run_date, locked_at, attempts
                  FROM jobs
                  WHERE run_date <= ?
            |]

getRunningJobs :: DBT IO (Vector Job)
getRunningJobs = query Select q ()
  where q = [sql| SELECT id, payload, created_at, run_date, locked_at, attempts
                  FROM jobs
                  WHERE locked_at IS NOT NULL
            |]

createJob :: Job -> DBT IO (Only Int)
createJob job = queryOne Insert q job
  where q = [sql| INSERT INTO jobs
                  (id, payload, created_at, run_date, locked_at, attempts)
                  VALUES (DEFAULT,?,?,?,?,?)
                  RETURNING id
            |]

deleteJob :: Int -> DBT IO ()
deleteJob jobId = execute Delete q (Only jobId)
  where q = [sql| DELETE FROM jobs
                  WHERE id = ?
            |]

getJob :: Int -> DBT IO Job
getJob jobId = queryOne Select q (Only jobId)
  where q = [sql| SELECT id, payload, created_at, run_date, locked_at, attempts
                  FROM jobs
                  WHERE id = ?
            |]

lockJob :: Int -> UTCTime -> DBT IO ()
lockJob jobId timestamp = execute Update q (timestamp, jobId)
  where q = [sql| UPDATE jobs as j SET locked_at = ?
                  WHERE j.id = ?
            |]

unlockJob :: Int -> DBT IO ()
unlockJob jobId = execute Update q (Only jobId)
  where q = [sql| UPDATE jobs as j SET locked_at = NULL
                  WHERE j.id = ?
            |]

isJobLocked :: Int -> DBT IO (Only Bool)
isJobLocked jobId = queryOne Select q (Only jobId)
  where q = [sql| SELECT
                    CASE WHEN locked_at IS NULL then false
                         ELSE true
                     END
                   FROM jobs WHERE id = ?
            |]
