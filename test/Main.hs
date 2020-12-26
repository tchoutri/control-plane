module Main (main) where

import Test.Hspec

import qualified Model.NotificationSpec as NotificationSpec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Notifiation model specs" NotificationSpec.spec
