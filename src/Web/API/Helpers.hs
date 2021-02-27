module Web.API.Helpers where

import           Control.Monad.Except (throwError)
import Web.Scotty.Internal.Types (ActionError (..))
import qualified Data.Aeson                 as A
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans

import DB.Types
import Web.Types

parseParam :: (Read a) => LText -> ActionT LText WebM (Either InternalError a)
parseParam paramName = do
  result <- param paramName
  case readMaybe result of
    Just a ->  pure $ Right a
    Nothing -> pure $ Left (DeserialisationError (toText paramName))

tryAction :: ActionT LText WebM (Either InternalError a)
          -> ActionT LText WebM a
tryAction action = do
  result <- action
  case result of
    Right a -> pure a
    Left err -> do
      let txtError = decodeUtf8 $ A.encode err
      throwError $ ActionError status500 txtError 
