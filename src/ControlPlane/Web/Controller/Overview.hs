module ControlPlane.Web.Controller.Overview where

import Lucid
import Web.Spock       (ActionCtxT)
import Web.Spock.Lucid (lucid)

import ControlPlane.Web.View.Base

handler :: (MonadIO m) => ActionCtxT ctx m a
handler = lucid $ template $ h1_ "System Overview" 
