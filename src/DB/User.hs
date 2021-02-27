{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB.User where

import           Data.Aeson                           (FromJSON (..), ToJSON (..))
import           Data.Password.Argon2                 (Argon2, Password, PasswordCheck (..), PasswordHash)
import qualified Data.Password.Argon2                 as Argon2
import           Data.Time                            (UTCTime)
import           Data.UUID                            (UUID)
import           Database.PostgreSQL.Simple           (Only (..))
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..))
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import           Database.PostgreSQL.Simple.ToRow     (ToRow (..))
import           Database.PostgreSQL.Transact         (DBT)
import           GHC.TypeLits                         (ErrorMessage (..), TypeError)

import           DB.Helpers              

newtype UserId = UserId { getUserId :: UUID }
  deriving stock (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON, FromField, ToField)

data User
  = User { userId      :: UserId
         , username    :: Text
         , displayName :: Text
         , password    :: PasswordHash Argon2
         , createdAt   :: UTCTime
         , updatedAt   :: UTCTime
         } deriving stock (Eq, Show, Generic)
           deriving anyclass (ToRow, FromRow)

data UserInfo
  = UserInfo { userId      :: UserId
             , username    :: Text
             , displayName :: Text
             } deriving stock (Eq, Show, Generic)
               deriving anyclass (ToJSON)

data NewUser
  = NewUser { username    :: Text
            , displayName :: Text
            , password    :: Password
            } deriving stock (Show, Generic)
              deriving anyclass (FromJSON)

-- | Type error! Do not use 'toJSON' on a 'Password'!
instance TypeError (ErrMsg "JSON") => ToJSON Password where
  toJSON = error "unreachable"

type ErrMsg e = 'Text "Warning! Tried to convert plain-text Password to " ':<>: 'Text e ':<>: 'Text "!"
          ':$$: 'Text "  This is likely a security leak. Please make sure whether this was intended."
          ':$$: 'Text "  If this is intended, please use 'unsafeShowPassword' before converting to " ':<>: 'Text e
          ':$$: 'Text ""

instance FromJSON Password where
  parseJSON = fmap Argon2.mkPassword . parseJSON

deriving via Text instance ToField (PasswordHash a)
deriving via Text instance FromField (PasswordHash a)

hashPassword :: (MonadIO m) => Password -> m (PasswordHash Argon2)
hashPassword = Argon2.hashPassword 

validatePassword :: Password -> PasswordHash Argon2 -> Bool
validatePassword inputPassword hashedPassword =
  Argon2.checkPassword inputPassword hashedPassword == PasswordCheckSuccess

insertUser :: User -> DBT IO ()
insertUser user = execute Insert q user
  where q = [sql| INSERT INTO users
                  (user_id, username, display_name, password, created_at, updated_at)
                  VALUES (?,?,?,?,?,?) |]

getUserById :: UserId -> DBT IO User
getUserById userId = queryOne Select q (Only userId)
  where q = [sql| SELECT user_id, username, display_name, password, created_at, updated_at
                  FROM users
                  WHERE user_id = ? |]

getUserByUsername :: Text -> DBT IO User
getUserByUsername username = queryOne Select q (Only username)
  where q = [sql| SELECT user_id, username, display_name, password, created_at, updated_at
                  FROM users
                  WHERE username = ? |]

deleteUser :: UserId -> DBT IO ()
deleteUser userId = execute Delete q (Only userId)
  where q = [sql| DELETE FROM users
                  WHERE user_id = ? |]
