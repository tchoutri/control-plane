module ControlPlane.Web.Types where

import ControlPlane.Environment

newtype ControlPlaneM a 
    = ControlPlaneM { getControlPlane :: ReaderT ControlPlaneEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader ControlPlaneEnv
                   )

runControlPlaneM :: ControlPlaneEnv -> ControlPlaneM a -> IO a
runControlPlaneM env action =
  runReaderT (getControlPlane action) env
