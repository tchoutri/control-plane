module DB.NotificationSpec where

import Relude.Unsafe (read)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldNotBe, shouldReturn)

import DB.Helpers (migrate)
import DB.Notification (Notification (..), NotificationId (..), NotificationStatus (..), getNotificationById,
                        insertNotification, readNotification)

notification1 :: Notification
notification1 =
  let notificationId = NotificationId (read "42b6c7e1-4779-46f4-895c-f6aa4e04f35d")
      device  = "Server1"
      title   = "Echoes from other world"
      message = "As the boundaries start to swirl"
      status  = NotificationUnread
      receivedAt = read "2021-01-09 13:22:09 UTC"
  in Notification{..}

notification2 :: Notification
notification2 =
  let notificationId = NotificationId (read "c247fd12-7f9c-11eb-b4b7-5405db82c3cd")
      device  = "laptop"
      title   = "Batterie faible"
      message = "Batterie en desous de 20%"
      status  = NotificationUnread
      receivedAt = read "2021-01-09 16:21:09 UTC"
  in Notification{..}

spec :: Spec
spec = describeDB migrate "Notification DB" $ do
  itDB "Insert" $ do
    insertNotification notification1
    insertNotification notification2
    getNotificationById (notificationId notification1)
      `shouldReturn` notification1
  itDB "Update notification" $ do
    let nid = notificationId notification2
    readNotification nid
    result <- getNotificationById nid
    status result `shouldNotBe` NotificationUnread
