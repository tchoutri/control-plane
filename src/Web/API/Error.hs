module Web.API.Error where

import Web.Scotty.Trans
import Network.HTTP.Types.Status

import DB.Types
import Web.Types

deserialisationError :: LText -> ActionT LText WebM ()
deserialisationError p = json $ DeserialisationError (toText p)

err500 :: InternalError -> ActionT LText WebM ()
err500 err = do
  status status500
  json err
