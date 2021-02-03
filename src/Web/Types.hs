module Web.Types where

import Environment (ControlPlaneEnv)

newtype WebM a
  = WebM { getWeb :: ReaderT ControlPlaneEnv IO a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader ControlPlaneEnv)

runWebM :: ControlPlaneEnv -> WebM a -> IO a
runWebM env action =
  runReaderT (getWeb action) env
