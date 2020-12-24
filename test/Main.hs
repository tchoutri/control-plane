module Main (main) where

import Test.Hspec

import qualified NotificationModelSpec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Notifiation model specs" NotificationModelSpec.spec
