{-# LANGUAGE QuasiQuotes #-}
module Model.Notification where

import Test.Hspec
import Data.UUID (nil)
import Data.Maybe (fromJust)
import Data.Aeson.Encode.Pretty
import Data.String.QQ

import DB.Notification

spec :: Spec
spec = do
  it "JSON encoding of Notification" $ do
    let nid = NotificationId nil
    let time = fromJust . readMaybe $ "2020-12-24 10:26:34.815690388 UTC"
    let n1 = Notification { notificationId = nid
                  , device     = "Telephone"
                  , title      = "Notification!"
                  , message    = "Bouh!"
                  , status     = NotificationUnread
                  , receivedAt = time
                  }
    let expectedPayload = [s|
{
    "status": {
        "status": "unread"
    },
    "message": "Bouh!",
    "title": "Notification!",
    "device": "Telephone",
    "receivedAt": "2020-12-24T10:26:34.815690388Z",
    "notificationId": "00000000-0000-0000-0000-000000000000"
}|]
    encodePretty n1 `shouldBe` expectedPayload
