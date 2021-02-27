module Web.Router where

import Prelude hiding (get)

import Network.Wai.Middleware.Cors          (simpleCors)
import Network.Wai.Middleware.CSP           (cspHeaders)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (static)
import Web.Scotty.Trans

import qualified Web.Controller.API  as API
import qualified Web.Controller.Root as Root
import           Web.Types

router :: ScottyT LText WebM ()
router = do
  middleware logStdoutDev
  middleware static
  middleware simpleCors
  middleware cspHeaders

  get "/" Root.handler
  get "/api/jobs" API.jobs
  get "/api/jobs/:jobId" API.job
  get "/api/notifications" API.notifications
