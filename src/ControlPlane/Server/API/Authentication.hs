module ControlPlane.Server.API.Authentication where

import Data.Password.Argon2       (mkPassword)
import Database.PostgreSQL.Simple (Only (..))
import Colourista.IO              (cyanMessage, redMessage)
import Servant.API.BasicAuth
import Servant.Server (BasicAuthCheck (..), BasicAuthResult (..))

import ControlPlane.DB.Types
import ControlPlane.DB.User  (User (..), validatePassword, getUserByUsername)

loginCheck :: ConnectionPool -> BasicAuthCheck User
loginCheck pool = BasicAuthCheck $ \basicAuthData -> do
  let username = decodeUtf8 . basicAuthUsername $ basicAuthData
  result <- getUserByUsername pool username
  case result of
    Left _ -> do
      redMessage $ "[AUTH] Authentication failed for user " <> username
      pure Unauthorized
    Right (Only user) ->
      let suppliedPassword = mkPassword (decodeUtf8 . basicAuthPassword $ basicAuthData)
       in if validatePassword suppliedPassword (password user)
          then do
            cyanMessage $ "[AUTH] Autentication successful for user " <> username
            pure $ Authorized user
          else do
            redMessage $ "[AUTH] Authentication failed for user " <> username
            pure Unauthorized
