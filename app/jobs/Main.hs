module Main where

import Data.Functor (void)

import Job (startJobService)

main :: IO ()
main = void startJobService
