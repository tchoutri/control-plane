module Web.Controller.Root 
  ( handler) where

import Lucid
import Web.Scotty.Trans

import DB.Helpers
import Job.DB
import Environment
import Web.Types
import Web.View.Root (view)
import Web.Errors

handler :: ActionT LText WebM ()
handler = do
  pool <- asks pgPool
  result <- liftIO $ runDB pool getAllJobs
  case result of
    Right jobs  -> html $ renderText $ view jobs
    Left errMsg -> err500 (show errMsg)
