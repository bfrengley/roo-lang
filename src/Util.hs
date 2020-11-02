module Util where

import Control.Monad (forM_, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

-- | The 'Equivalent' class defines the concept of "loose" equality, as opposed to the strict
-- mathematical equality defined by 'Eq'. This is primarily useful where you may want to define a
-- looser concept of equality for a type while still being able to use 'Eq' (such as for types
-- which may be keys of a map). An example case where loose equality is useful is a bottom type,
-- which, while mathematically it should be equal to other types, probably shouldn't be treated as
-- the same key in a map.
class Equivalent a where
  (=%=) :: a -> a -> Bool

(=>>) :: Monad m => m b -> (b -> m ()) -> m b
a =>> f = do
  val <- a
  f val
  return val

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- | Execute a monad action on the contents of a 'Maybe a' if it contains a value; otherwise do
-- nothing. This is just an alias to make it more clear what's actually happening, since folding
-- a 'Maybe' isn't very intuitive --- it should get inlined by GHC.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

-- | Lifts a 'Maybe b' inside a monad 'm' into a 'MaybeT m b'.
liftToMaybeT :: Monad m => m (Maybe b) -> MaybeT m b
liftToMaybeT = lift >=> liftMaybe
