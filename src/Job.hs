module Job (startJobService) where

import Colourista.IO            (cyanMessage, greenMessage)
import Control.Concurrent       (threadDelay)
import Data.Maybe               (fromJust)
import Data.Time                (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)

import DB.Helpers  (runDB)
import Job.DB      (Job (..), Payload (CheckWebsite, GrabJSON), Website (Website), createJob, deleteJob,
                                 getJobs, lockJob, isJobLocked, unlockJob)
import Environment (ControlPlaneEnv (..), mkEnv)
import Job.Runner  (JobRunner, startJobRunner)
import Job.Model   (JobInfo (JobInfo), mkJob)
import Database.PostgreSQL.Simple (Only(Only))

startJobService :: IO ()
startJobService = do
  greenMessage "[+] Starting job worker"
  env <- mkEnv
  forever $ do
    threadDelay $ 600 * 100000 -- 1 minute
    startJobRunner env processJobs

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
      void $ jobRunner payload
      jobUnlocker $ fromJust $ jobId job
    _                     -> pure ()

jobRunner :: Payload -> JobRunner IO ()
jobRunner (CheckWebsite w) = checkWebsite w
jobRunner (GrabJSON t)     = grabJSON t

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
