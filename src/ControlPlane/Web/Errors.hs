module ControlPlane.Web.Errors where

import Lucid
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import ControlPlane.Web.View.Base
import ControlPlane.Web.Types

err500 :: Text -> ActionT LText ControlPlaneM ()
err500 message = do
  html $ renderText $ template $ do
    h1_ "Error 500"
    p_ (toHtml message)
  status status500
