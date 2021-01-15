module ControlPlane.Server (startService) where

import Control.Concurrent.Async

import ControlPlane.Web (startWebService)
import ControlPlane.Job (startJobService)

startService :: IO ()
startService =
  withAsync startWebService $ \webAsync ->
    withAsync startJobService $ \jobAsync ->
      void $ waitAny [webAsync, jobAsync]
