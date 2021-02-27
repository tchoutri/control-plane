module Web.Types where

import Environment (ControlPlaneEnv)

newtype WebM a 
    = WebM { getWeb :: ReaderT ControlPlaneEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader ControlPlaneEnv
                   )

runWebM :: ControlPlaneEnv -> WebM a -> IO a
runWebM env action =
  runReaderT (getWeb action) env
