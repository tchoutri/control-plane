{- See https://github.com/kowainik/cake-slayer/blob/744f072c0eeaf50e43210e4b548705e1948e5a39/src/CakeSlayer/Has.hs
  for more information.
-}
module ControlPlane.Environment.Has
  ( Has (..)
  , grab
  ) where

import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)

class Has field env where
  obtain :: env -> field

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab#-}

newtype Field (s :: Symbol) env = Field { unField :: env }

instance forall s f env. (HasField s env f) => Has f (Field s env) where
  obtain :: Field s env -> f
  obtain = getField @s . unField
  {-# INLINE obtain #-}
