module ControlPlane.Model.User where

import ControlPlane.DB.User

toUserInfo :: User -> UserInfo
toUserInfo User{..} = UserInfo{..}
