module DB.Notification where

import Database.PostgreSQL.Simple   (Only (..))
import Relude.Unsafe                (read)
import Test.Hspec
import Test.Hspec.DB

-- import ControlPlane.DB.Helpers
import ControlPlane.DB.Notification (Notification (..), NotificationId (..), NotificationStatus (..), getNotificationById, insertNotification)
import DB.Helpers

notification1 :: Notification
notification1 = 
  let notificationId = NotificationId (read "42b6c7e1-4779-46f4-895c-f6aa4e04f35d")
      device  = "Server1"
      title   = "Echoes from other world"
      message = "As the boundaries start to swirl"
      status  = NotificationUnread
      receivedAt = read "2021-01-09 13:22:09 UTC"
  in Notification{..}

spec :: Spec
spec = describeDB migrate "Notification DB" $ do
  itDB "Insert notification" $ do
    insertNotification notification1
    result <- getNotificationById (notificationId notification1)
    pure $ result `shouldBe` Only notification1
