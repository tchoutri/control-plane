module Main where

import ControlPlane.Job (startJobService)

main :: IO ()
main = startJobService
