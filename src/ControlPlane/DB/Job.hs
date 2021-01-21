{-# LANGUAGE QuasiQuotes #-}
module ControlPlane.DB.Job where

import Data.Aeson
import Data.Time
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Transact (DBT)

import ControlPlane.DB.Helpers 

data Job
  = Job { jobId     :: Maybe Int
        , payload   :: Payload
        , createdAt :: UTCTime
        , runDate   :: UTCTime
        , lockedAt :: Maybe UTCTime
        , attempts  :: Integer
        } deriving stock (Eq, Show, Generic)
          deriving anyclass (FromJSON, ToJSON, FromRow)

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

getAllJobs :: DBT IO [Job]
getAllJobs = queryMany Select q ()
  where q = [sql| SELECT id, payload, created_at, run_date, locked_at, attempts
                  FROM jobs
            |]

getJobs :: UTCTime -> DBT IO [Job]
getJobs currentTime = queryMany Select q (Only currentTime)
  where q = [sql| SELECT id, payload, created_at, run_date, locked_at, attempts
                  FROM jobs
                  WHERE run_date <= ?
            |]

getRunningJobs :: DBT IO [Job]
getRunningJobs = queryMany Select q ()
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
