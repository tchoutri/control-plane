module Main (main) where

import Test.Hspec

import qualified Model.Notification as NotificationModel
import qualified DB.Notification as NotificationDB

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Notifiation model specs" NotificationModel.spec
  NotificationDB.spec
