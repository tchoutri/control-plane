module Main where

import ControlPlane.Server.API (routeLayout)

main :: IO ()
main = putTextLn $ routeLayout
