module Web.Controller.Overview where

import Lucid
import Web.Scotty.Trans

import Web.View.Base

handler :: (Monad m, ScottyError e) => ActionT e m ()
handler = html $ renderText $ template $ h1_ "System Overview"
