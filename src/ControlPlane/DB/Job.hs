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
  = Job { jobId     :: Maybe Int64
        , payload   :: Payload
        , createdAt :: UTCTime
        , runDate   :: UTCTime
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
  toRow Job{..} = toRow (payload, createdAt, runDate)

getJobs :: UTCTime -> DBT IO [Job]
getJobs currentTime = queryMany q (Only currentTime)
  where q = [sql| SELECT id, payload, created_at, run_date
                  FROM jobs
                  WHERE run_date <= ?
            |]

createJob :: Job -> DBT IO ()
createJob job = execute q job
  where q = [sql| INSERT INTO jobs
                  (payload, created_at, run_date)
                  VALUES (?,?,?)
            |]
