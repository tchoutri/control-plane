module ControlPlane.Web.Router where

import Prelude hiding (get)

import Network.Wai.Middleware.Cors          (simpleCors)
import Network.Wai.Middleware.CSP           (cspHeaders)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (static)
import Web.Scotty.Trans

import ControlPlane.Web.Types
import qualified ControlPlane.Web.Controller.Root as Root

router :: ScottyT LText ControlPlaneM ()
router = do
  middleware logStdoutDev
  middleware static
  middleware simpleCors
  middleware cspHeaders

  get "/" Root.handler
