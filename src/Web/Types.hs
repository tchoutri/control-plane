module Web.Types where

import Database.PostgreSQL.Entity.DBT (ConnectionPool)
import Web.Scotty.Internal.Types (ActionT)
import Web.Session

newtype WebM a
  = WebM { getWeb :: ReaderT WebEnvironment IO a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader WebEnvironment)

data WebEnvironment
  = WebEnvironment { dbPool :: ConnectionPool
                   , sessions :: ScottySM UserAssigns
                   }

printAssigns :: ActionT LText WebM ()
printAssigns = do
  sm <- readSession =<< asks sessions
  print sm

runWebM :: WebEnvironment -> WebM a -> IO a
runWebM env action =
  runReaderT (getWeb action) env
