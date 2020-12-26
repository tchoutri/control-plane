{-# LANGUAGE QuasiQuotes #-}
module Model.NotificationSpec where

import Test.Hspec
import Data.UUID (nil)
import Data.Maybe (fromJust)
import Data.Aeson.Encode.Pretty
import Data.String.QQ

import ControlPlane.DB.Notification

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
    "receivedAt": "2020-12-24T10:26:34.815690388Z",
    "device": "Telephone",
    "title": "Notification!",
    "notificationId": "00000000-0000-0000-0000-000000000000",
    "message": "Bouh!"
}|]

    encodePretty n1 `shouldBe` expectedPayload