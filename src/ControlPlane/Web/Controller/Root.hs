module ControlPlane.Web.Controller.Root 
  ( handler) where

import Web.Spock       (ActionCtxT)
import Web.Spock.Lucid (lucid)

import ControlPlane.Web.View.Root (view)

handler :: (MonadIO m) => ActionCtxT ctx m a
handler = lucid view
