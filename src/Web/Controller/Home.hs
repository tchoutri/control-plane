module Web.Controller.Home where

import Database.PostgreSQL.Entity.DBT (runDB)
import Web.Scotty.Trans (ActionT, html)

import Environment (ControlPlaneEnv (..))
import Job.DB (getAllJobs)
import Web.Error (err500)
import Web.Types (WebM)
import Web.View.Home (view)

homeShow :: ActionT LText WebM ()
homeShow = do
  pool <- asks pgPool
  dbResult <- liftIO $ runDB pool getAllJobs
  case dbResult of
    Left errMsg -> err500 (show errMsg)
    Right jobs  -> do
     result <- lift $ view jobs
     html (toLText result)
