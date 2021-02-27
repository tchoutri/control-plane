module Model.User where

import DB.User

toUserInfo :: User -> UserInfo
toUserInfo User{..} = UserInfo{..}
