module Job (startJobService) where

import Colourista.IO (greenMessage)
import Control.Concurrent (threadDelay)

import Environment (mkEnv)
import Job.Runner (processJobs, startJobRunner)

startJobService :: IO ()
startJobService = do
  greenMessage "[+] Starting job worker"
  env <- mkEnv
  void $ infinitely $ do
    threadDelay $ 600 * 100000 -- 1 minute
    startJobRunner env processJobs
