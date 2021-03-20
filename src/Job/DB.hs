{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Job.DB where

import Data.Aeson (FromJSON (parseJSON), Options (sumEncoding),
                   SumEncoding (TaggedObject, contentsFieldName, tagFieldName), ToJSON (toJSON), defaultOptions,
                   genericParseJSON, genericToJSON)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity (..), _select, delete, selectById, selectWhereNotNull, updateFieldsBy)
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert, Select, Update), execute, query, queryOne, query_)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Transact (DBT)
import Web.Scotty.Trans (Parsable)

data Job
  = Job { jobId     :: Maybe Int
        , payload   :: Payload
        , createdAt :: UTCTime
        , runDate   :: UTCTime
        , lockedAt  :: Maybe UTCTime
        , attempts  :: Integer
        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, FromRow, ToJSON)

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

newtype Website
  = Website { url :: Text }
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON, Parsable)

data Payload
  = CheckWebsite Website
  deriving stock (Eq, Generic, Show)

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
  where q = [sql| SELECT job_id, payload, created_at, run_date, locked_at, attempts
                  FROM jobs
                  WHERE run_date <= ?
            |]

getRunningJobs :: DBT IO (Vector Job)
getRunningJobs = selectWhereNotNull @Job ["locked_at"]

createJob :: Job -> DBT IO (Only Int)
createJob job = queryOne Insert q job
  where q = [sql| INSERT INTO jobs
                  (job_id, payload, created_at, run_date, locked_at, attempts)
                  VALUES (DEFAULT,?,?,?,?,?)
                  RETURNING job_id
            |]

deleteJob :: Int -> DBT IO ()
deleteJob jobId = delete @Job (Only jobId)

getJob :: Int -> DBT IO Job
getJob jobId = selectById @Job (Only jobId)

lockJob :: Int -> UTCTime -> DBT IO ()
lockJob jobId timestamp = void $ updateFieldsBy @Job ["locked_at"] (primaryKey @Job, jobId) (Only timestamp)

unlockJob :: Int -> DBT IO ()
unlockJob jobId = void $ execute Update q (Only jobId)
  where q = [sql| UPDATE jobs as j SET locked_at = NULL
                  WHERE j.job_id = ?
            |]

isJobLocked :: Int -> DBT IO (Only Bool)
isJobLocked jobId = queryOne Select q (Only jobId)
  where q = [sql| SELECT
                    CASE WHEN locked_at IS NULL then false
                         ELSE true
                     END
                   FROM jobs WHERE job_id = ?
            |]
