module ControlPlane.Web.Controller.Overview where

import Web.Scotty.Trans
import Lucid

import ControlPlane.Web.View.Base

handler :: (Monad m, ScottyError e) => ActionT e m ()
handler = html $ renderText $ template $ h1_ "System Overview" 
