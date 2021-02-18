module ControlPlane.Web.Controller.API where

import Web.Scotty.Trans (ActionT, json)

import ControlPlane.DB.Helpers
import ControlPlane.Job.DB
import ControlPlane.DB.Notification
import ControlPlane.Environment
import ControlPlane.Web.API.Helpers
import ControlPlane.Web.Errors
import ControlPlane.Web.Types

jobs :: ActionT LText ControlPlaneM ()
jobs = do
  pool <- asks pgPool
  result <- liftIO $ runDB pool getAllJobs
  case result of
    Right dbJobs  -> json dbJobs
    Left errMsg -> err500 (show errMsg)

job :: ActionT LText ControlPlaneM ()
job = do
  jobId <- tryAction $ parseParam "jobId"
  pool <- asks pgPool
  result <- liftIO $ runDB pool (getJob jobId)
  case result of
    Right dbJobs -> json dbJobs
    Left errMsg -> err500 (show errMsg)

notifications :: ActionT LText ControlPlaneM ()
notifications = do
  pool <- asks pgPool
  result <- liftIO $ runDB pool getNotifications
  case result of
    Right notifs -> json notifs
    Left errMsg -> err500 (show errMsg)
