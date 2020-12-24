module Main (main) where

import ControlPlane.Server.API (startService)

main :: IO ()
main = startService
