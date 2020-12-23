module ControlPlane.DB where

import Data.Time

import ControlPlane.DB.Types

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections = 
  createPool (connect connectInfo) close subPools timeout connections
