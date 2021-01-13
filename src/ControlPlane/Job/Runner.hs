module ControlPlane.Job.Runner where

import Control.Monad.Except (MonadError)

import ControlPlane.Environment (ControlPlaneEnv)

newtype JobRunner m a
  = JobRunner { getJobRunner :: (ReaderT ControlPlaneEnv (ExceptT Text m)) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader ControlPlaneEnv
                   , MonadError Text
                   )

startJobRunner :: ControlPlaneEnv -> JobRunner m a -> m (Either Text a)
startJobRunner env action =
  runExceptT $ runReaderT (getJobRunner action) env
