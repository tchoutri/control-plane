module ControlPlane.Web.API.Error where

import Web.Scotty.Trans
import Network.HTTP.Types.Status

import ControlPlane.DB.Types
import ControlPlane.Web.Types

deserialisationError :: LText -> ActionT LText ControlPlaneM ()
deserialisationError p = json $ DeserialisationError (toText p)

err500 :: InternalError -> ActionT LText ControlPlaneM ()
err500 err = do
  status status500
  json err
