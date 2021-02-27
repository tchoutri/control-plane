module Job.Runner where

import Colourista.IO (cyanMessage)
import Control.Monad.Except (MonadError)
import Data.Maybe (fromJust)
import Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)

import DB.Helpers (runDB)
import Environment (ControlPlaneEnv (..))
import Job.DB (Job (..), Payload (CheckWebsite, GrabJSON), Website (Website), createJob, deleteJob, getJobs)
import Job.Model (JobInfo (JobInfo), mkJob)

newtype JobRunner m a
  = JobRunner { getJobRunner :: (ReaderT ControlPlaneEnv (ExceptT Text m)) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader ControlPlaneEnv
                   , MonadError Text
                   )

startJobRunner :: ControlPlaneEnv -> JobRunner m a -> m (Either Text a)
startJobRunner env action =
  runExceptT $ runReaderT (getJobRunner action) env

processJobs :: JobRunner IO ()
processJobs = do
  pool <- asks pgPool
  ts <- liftIO getCurrentTime
  result <- liftIO $ runDB pool (getJobs ts)
  case result of
    Left err   -> print err >> pure ()
    Right jobs -> do
      let formattedTs = formatTime defaultTimeLocale "%F %H:%M:%S" ts
      liftIO $ cyanMessage $ "[" <> toText formattedTs <> "] Fetched " <> show (length jobs) <> " jobs"
      mapM_ processJob jobs
      mapM_ deleteAndRequeue jobs

processJob :: Job -> JobRunner IO ()
processJob job = do
  case payload job of
    CheckWebsite w -> checkWebsite w
    GrabJSON t     -> grabJSON t

deleteAndRequeue :: Job -> JobRunner IO ()
deleteAndRequeue Job{..} = do
  pool <- asks pgPool
  ts <- liftIO getCurrentTime
  let threeMinutes = 3 * 60 :: NominalDiffTime
  liftIO $ runDB pool $ deleteJob (fromJust jobId)
  newJob <- liftIO $ mkJob $ JobInfo payload (addUTCTime threeMinutes ts)
  void $ liftIO $ runDB pool $ createJob newJob

checkWebsite :: Website -> JobRunner IO ()
checkWebsite (Website url) =
  putTextLn $ "Checking website " <> url

grabJSON :: Text -> JobRunner IO ()
grabJSON  t =
  putTextLn $ "Grabbing JSON " <> t
