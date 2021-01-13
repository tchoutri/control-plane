module ControlPlane.Web.Types where

data MySession = EmptySession
newtype MyAppState = AppState (MVar Int)
