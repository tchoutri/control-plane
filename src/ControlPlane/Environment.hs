module ControlPlane.Environment where

import           Data.Time                  (NominalDiffTime)
import qualified Database.PostgreSQL.Simple as PG
import           Env
import           Prelude                    hiding (Reader)

import           ControlPlane.DB.Helpers
import           ControlPlane.DB.Types

-- *Env datatypes are parsed as-is from the outside
data ControlPlaneConfig
  = ControlPlaneConfig { pgConfig   :: PG.ConnectInfo
                       , poolConfig :: PoolConfig
                       } deriving (Show)

data PoolConfig
  = PoolConfig { subPools          :: Int
               , connectionTimeout :: NominalDiffTime
               , connections       :: Int
               } deriving (Show)

getConfig :: IO ControlPlaneConfig
getConfig = Env.parse id parseConfig

parseConfig :: Parser Error ControlPlaneConfig
parseConfig =
  ControlPlaneConfig <$> parseConnectInfo
                     <*> parsePoolConfig

parseConnectInfo :: Parser Error PG.ConnectInfo
parseConnectInfo =
  PG.ConnectInfo <$> var str  "DB_HOST"     (help "PostgreSQL host")
                 <*> var port "DB_PORT"     (help "PostgreSQL port")
                 <*> var str  "DB_USER"     (help "PostgreSQL user")
                 <*> var str  "DB_PASSWORD" (help "PostgreSQL password")
                 <*> var str  "DB_DATABASE" (help "Control-Plane database")

parsePoolConfig :: Parser Error PoolConfig
parsePoolConfig =
  PoolConfig <$> var (int >=> nonNegative) "DB_SUB_POOLS"        (help "Number of sub-pools")
             <*> var timeout               "DB_TIMEOUT"          (help "Timeout for each connection")
             <*> var (int >=> nonNegative) "DB_POOL_CONNECTIONS" (help "Number of connections per sub-pool")

-- *Env datatypes are the ones that are passed in the application
newtype ControlPlaneEnv
  = ControlPlaneEnv { pgPool :: ConnectionPool
                    } deriving (Show)

mkControlPlaneEnv :: IO ControlPlaneEnv
mkControlPlaneEnv = do
  ControlPlaneConfig{..} <- getConfig
  let PoolConfig{..} = poolConfig
  pgPool <- mkPool pgConfig subPools connectionTimeout connections
  pure $ ControlPlaneEnv{..}

-- Env parser helpers

int :: Reader Error Int
int i =
  case readMaybe i of
    Nothing -> Left . unread . show $ i
    Just i' -> Right i'

port :: Reader Error Word16
port p =
  case int p of
    Left err -> Left err
    Right intPort ->
      if intPort >= 1 && intPort <= 65535
      then Right $ fromIntegral intPort
      else Left . unread . show $ p

nonNegative :: Int -> Either Error Int
nonNegative nni =
  if nni >= 0
  then Right nni
  else Left . unread . show $ nni

bool :: Reader Error Bool
bool b =
  case readMaybe b of
    Nothing -> Left . unread . show $ b
    Just b' -> Right b'

timeout :: Reader Error NominalDiffTime
timeout t = second fromIntegral (int >=> nonNegative $ t)
