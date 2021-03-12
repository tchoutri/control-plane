module Job.Runner where

import Colourista.IO (cyanMessage)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadError)
import Data.Maybe (fromJust)
import Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Database.PostgreSQL.Entity.DBT (runDB)
import Database.PostgreSQL.Simple (Only (..))

import Environment (ControlPlaneEnv (..))
import Job.DB (Job (..), Payload (CheckWebsite, GrabJSON), Website (Website), createJob, deleteJob, getJobs,
               isJobLocked, lockJob, unlockJob)
import Job.Model (JobInfo (JobInfo), mkJob)

newtype JobRunner m a
  = JobRunner { getJobRunner :: (ReaderT ControlPlaneEnv (ExceptT Text m)) a }
  deriving newtype (Applicative, Functor, Monad, MonadError Text, MonadIO, MonadReader ControlPlaneEnv)

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
  result <- jobLocker job
  case result of
    Just payload -> do
      void $ runJob payload
      jobUnlocker $ fromJust $ jobId job
    _ -> pure ()

runJob :: Payload -> JobRunner IO ()
runJob (CheckWebsite w) = checkWebsite w
runJob (GrabJSON t)     = grabJSON t

deleteAndRequeue :: Job -> JobRunner IO ()
deleteAndRequeue Job{..} = do
  pool <- asks pgPool
  ts <- liftIO getCurrentTime
  let threeMinutes = 3 * 60 :: NominalDiffTime
  liftIO $ runDB pool $ deleteJob (fromJust jobId)
  newJob <- liftIO $ mkJob $ JobInfo payload (addUTCTime threeMinutes ts)
  void $ liftIO $ runDB pool $ createJob newJob

jobLocker :: Job -> JobRunner IO (Maybe Payload)
jobLocker job = do
  let i = fromJust $ jobId job
  liftIO $ cyanMessage $ "[Job] Locking job " <> show i
  pool <- asks pgPool
  ts <- liftIO getCurrentTime
  result <- liftIO $ runDB pool $ isJobLocked i
  case result of
    Left err   -> print err >> pure Nothing
    Right (Only True) -> pure Nothing
    Right (Only False) -> do
      void $ liftIO $ runDB pool $ lockJob i ts
      pure $ Just (payload job)

jobUnlocker :: Int -> JobRunner IO ()
jobUnlocker i = do
  liftIO $ cyanMessage $ "[Job] Unlocking job " <> show i
  pool <- asks pgPool
  liftIO $ void $ runDB pool $ unlockJob i

checkWebsite :: Website -> JobRunner IO ()
checkWebsite (Website url) = do
  putTextLn $ "Checking website " <> url
  liftIO $ threadDelay 10000000
  putTextLn $ "Website checked!" <> url

grabJSON :: Text -> JobRunner IO ()
grabJSON  t =
  putTextLn $ "Grabbing JSON " <> t
