module ControlPlane.Job (startJobService) where

import Colourista.IO
import Control.Concurrent
import Data.Time
import Data.Maybe (fromJust)

import ControlPlane.DB.Helpers
import ControlPlane.DB.Job
import ControlPlane.Environment (ControlPlaneEnv (..), mkControlPlaneEnv)
import ControlPlane.Job.Runner
import ControlPlane.Model.Job

startJobService :: IO ()
startJobService = do
  greenMessage "[+] Starting job worker"
  env <- mkControlPlaneEnv
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
