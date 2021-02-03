module Web.Router where

import Prelude hiding (get)

import Network.Wai.Middleware.CSP (cspHeaders)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import Web.Scotty.Trans (ScottyT, get, middleware)

import qualified Web.Controller.API as API
import qualified Web.Controller.Home as Home
import Web.Types (WebM)

router :: ScottyT LText WebM ()
router = do
  middleware logStdoutDev
  middleware $ staticPolicy noDots -- for favicon.ico
  middleware simpleCors
  middleware cspHeaders

  get "/" Home.homeShow

  get "/api/jobs" API.jobs
  get "/api/jobs/:jobId" API.job
  get "/api/notifications" API.notifications
