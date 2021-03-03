{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB.User where

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Password.Argon2 (Argon2, Password, PasswordCheck (..), PasswordHash)
import qualified Data.Password.Argon2 as Argon2
import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import           Database.PostgreSQL.Entity (Entity (..), delete, insert, selectById, selectOneByField)
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow (ToRow (..))
import           Database.PostgreSQL.Transact (DBT)
import           GHC.TypeLits (ErrorMessage (..), TypeError)
import Database.PostgreSQL.Simple (Only(Only))

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

instance Entity User where
  tableName = "users"
  primaryKey = "user_id"
  fields = [ "user_id"
           , "username"
           , "display_name"
           , "password"
           , "created_at"
           , "updated_at"
           ]

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
insertUser user = insert @User user

getUserById :: UserId -> DBT IO User
getUserById userId = selectById @User userId

getUserByUsername :: Text -> DBT IO User
getUserByUsername username = selectOneByField "username" username

deleteUser :: UserId -> DBT IO ()
deleteUser userId = delete @User (Only userId)
