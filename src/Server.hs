module Server (startService) where

import Web (startWebService)

startService :: IO ()
startService = startWebService
