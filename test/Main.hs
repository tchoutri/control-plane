module Main (main) where

import Test.Hspec

import qualified DB.JobSpec as JobSpec
import qualified DB.NotificationSpec as NotificationDB
import qualified Model.Notification as NotificationModel

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Notifiation model specs" NotificationModel.spec
  NotificationDB.spec
  JobSpec.spec

