module Web.Error where

import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Web.Types

err500 :: LText -> ActionT LText WebM ()
err500 message = do
  status status500
  html message

