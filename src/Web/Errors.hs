module Web.Errors where

import Lucid
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Web.View.Base
import Web.Types

err500 :: Text -> ActionT LText WebM ()
err500 message = do
  html $ renderText $ template $ do
    h1_ "Error 500"
    p_ (toHtml message)
  status status500
