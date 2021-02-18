module ControlPlane.Web.Controller.Root 
  ( handler) where

import Lucid
import Web.Scotty.Trans

import ControlPlane.DB.Helpers
import ControlPlane.Job.DB
import ControlPlane.Environment
import ControlPlane.Web.Types
import ControlPlane.Web.View.Root (view)
import ControlPlane.Web.Errors

handler :: ActionT LText ControlPlaneM ()
handler = do
  pool <- asks pgPool
  result <- liftIO $ runDB pool getAllJobs
  case result of
    Right jobs  -> html $ renderText $ view jobs
    Left errMsg -> err500 (show errMsg)
