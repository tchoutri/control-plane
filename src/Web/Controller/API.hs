module Web.Controller.API where

import Web.Scotty.Trans (ActionT, json)

import Database.PostgreSQL.Entity.DBT

import DB.Notification
import DB.Types
import Job.DB
import Web.API.Helpers
import Web.Types

jobs :: ActionT LText WebM ()
jobs = do
  pool <- asks dbPool
  result <- liftIO $ runDB pool getAllJobs
  case result of
    Right dbJobs -> json dbJobs
    Left err     -> json (InternalError err)

job :: ActionT LText WebM ()
job = do
  jobId <- tryAction $ parseParam "jobId"
  pool <- asks dbPool
  result <- liftIO $ runDB pool (getJob jobId)
  case result of
    Right dbJobs -> json dbJobs
    Left err     -> json (InternalError err)

notifications :: ActionT LText WebM ()
notifications = do
  pool <- asks dbPool
  result <- liftIO $ runDB pool getNotifications
  case result of
    Right notifs -> json notifs
    Left err     -> json (InternalError err)
