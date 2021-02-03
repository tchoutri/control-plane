module Web.API.Helpers where

import Control.Monad.Except (throwError)
import qualified Data.Aeson as A
import Network.HTTP.Types.Status (status500)
import Web.Scotty.Internal.Types (ActionError (..))
import Web.Scotty.Trans (ActionT, param)

import DB.Types (InternalError (..))
import Database.PostgreSQL.Entity.DBT.Types (DBError (DeserialisationError))
import Web.Types (WebM)

parseParam :: (Read a) => LText -> ActionT LText WebM (Either InternalError a)
parseParam paramName = do
  result <- param paramName
  case readMaybe result of
    Just a  ->  pure $ Right a
    Nothing -> pure $ Left (InternalError $ DeserialisationError (toText paramName))

tryAction :: ActionT LText WebM (Either InternalError a)
          -> ActionT LText WebM a
tryAction action = do
  result <- action
  case result of
    Right a -> pure a
    Left err -> do
      let txtError = decodeUtf8 $ A.encode err
      throwError $ ActionError status500 txtError
