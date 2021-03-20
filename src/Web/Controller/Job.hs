module Web.Controller.Job where

import Data.Time
import Database.PostgreSQL.Entity.DBT (runDB)
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import DB.Types (InternalError (InternalError))
import Job.DB
import Job.Model
import Web.Controller.FlashMessage
import Web.Error
import Web.Types
import Web.View.Job

index :: ActionT LText WebM ()
index = do
  pool <- asks dbPool
  dbResult <- liftIO $ runDB pool getAllJobs
  case dbResult of
    Left errMsg -> err500 (show errMsg)
    Right jobs  -> do
     result <- indexView jobs
     html result

new :: ActionT LText WebM ()
new = newView >>= html . toLText

create :: ActionT LText WebM ()
create = do 
  pool <- asks dbPool
  ts <- liftIO getCurrentTime 
  payload :: Text <- param "payload"
  status created201
  case payload of
    "CheckWebsite" -> do
      website :: Website <- param "url"
      let twoMinutes = 2 * 60 :: NominalDiffTime 
      j <- liftIO $ mkJob $ JobInfo (CheckWebsite website) (addUTCTime twoMinutes ts)
      result <- liftIO $ runDB pool $ createJob j
      case result of
        Right _ -> do
          putInfo "Job inserted!"
          redirect "/jobs"
        Left err -> do
          putError $ toText $ InternalError err
          redirect "/jobs"
    _ -> redirect "/jobs"

delete :: ActionT LText WebM ()
delete = do
  pool <- asks dbPool
  jId :: Int <- param "id"
  result <- liftIO $ runDB pool $ deleteJob jId
  case result of
    Right _ -> do
      putInfo "Job deleted!"
      redirect "/jobs"
    Left err -> do
      putError $ toText $ InternalError err
      redirect "/jobs"

-- patch :: ActionT LText WebM ()
-- patch = undefined
