module Web.Controller.API where

import Web.Scotty.Trans (ActionT, json)

import DB.Helpers
import Job.DB
import DB.Notification
import Environment
import Web.API.Helpers
import Web.Errors
import Web.Types

jobs :: ActionT LText WebM ()
jobs = do
  pool <- asks pgPool
  result <- liftIO $ runDB pool getAllJobs
  case result of
    Right dbJobs  -> json dbJobs
    Left errMsg -> err500 (show errMsg)

job :: ActionT LText WebM ()
job = do
  jobId <- tryAction $ parseParam "jobId"
  pool <- asks pgPool
  result <- liftIO $ runDB pool (getJob jobId)
  case result of
    Right dbJobs -> json dbJobs
    Left errMsg -> err500 (show errMsg)

notifications :: ActionT LText WebM ()
notifications = do
  pool <- asks pgPool
  result <- liftIO $ runDB pool getNotifications
  case result of
    Right notifs -> json notifs
    Left errMsg -> err500 (show errMsg)
