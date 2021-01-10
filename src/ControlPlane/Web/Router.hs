module ControlPlane.Web.Router where

import Prelude hiding (get)

import Database.PostgreSQL.Simple           (Connection)
import Network.Wai.Middleware.Cors          (simpleCors)
import Network.Wai.Middleware.CSP           (cspHeaders)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (static)
import Web.Spock                            (SpockM, get, middleware, root)

import ControlPlane.Web.Types
import qualified ControlPlane.Web.Controller.Root as Root
import qualified ControlPlane.Web.Controller.Overview as Overview

router :: SpockM Connection MySession MyAppState ()
router = do
  middleware logStdoutDev
  middleware static
  middleware simpleCors
  middleware cspHeaders
  get root Root.handler
  get "overview" Overview.handler
