module Web.API.Error where

import Network.HTTP.Types.Status
import Web.Scotty.Trans

import DB.Types
import Web.Types

deserialisationError :: LText -> ActionT LText WebM ()
deserialisationError p = json $ DeserialisationError (toText p)

err500 :: InternalError -> ActionT LText WebM ()
err500 err = do
  status status500
  json err
