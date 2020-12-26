{-# LANGUAGE StrictData #-}
module ControlPlane.Server.API.Types
  ( InternalError (..)
  , ControlPlaneM (..)
  , runControlPlaneM
  ) where

import Control.Monad.Except     (MonadError)
import Servant                  (ServerError)

import ControlPlane.DB.Types    (InternalError (..))
import ControlPlane.Environment

newtype ControlPlaneM m a 
    = ControlPlaneM { getControlPlane :: (ReaderT ControlPlaneEnv (ExceptT ServerError m)) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader ControlPlaneEnv
                   , MonadError ServerError
                   )

runControlPlaneM :: ControlPlaneEnv -> ControlPlaneM m a -> m (Either ServerError a)
runControlPlaneM env action =
  runExceptT $ runReaderT (getControlPlane action) env
