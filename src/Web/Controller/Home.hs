module Web.Controller.Home where

import Web.Scotty.Trans (ActionT)

import Web.Types (WebM)
import qualified Web.Controller.Job as Job

index :: ActionT LText WebM ()
index = Job.index
