module ControlPlane.Web.API.Helpers where

import           Control.Monad.Except (throwError)
import Web.Scotty.Internal.Types (ActionError (..))
import qualified Data.Aeson                 as A
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans

import ControlPlane.DB.Types
import ControlPlane.Web.Types

parseParam :: (Read a) => LText -> ActionT LText ControlPlaneM (Either InternalError a)
parseParam paramName = do
  result <- param paramName
  case readMaybe result of
    Just a ->  pure $ Right a
    Nothing -> pure $ Left (DeserialisationError (toText paramName))

tryAction :: ActionT LText ControlPlaneM (Either InternalError a)
          -> ActionT LText ControlPlaneM a
tryAction action = do
  result <- action
  case result of
    Right a -> pure a
    Left err -> do
      let txtError = decodeUtf8 $ A.encode err
      throwError $ ActionError status500 txtError 
