-- |
-- Module: Util
-- Description: This module defines utility functions used in the Roo compiler.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- This module defines several small utility functions used in various other places. They don't
-- belong in a specific other module but are reused, so they go here instead.
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

-- | Lift a 'Maybe a' which is not in a monad into a 'MaybeT'.
liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- | Lifts a 'Maybe b' inside a monad 'm' into a 'MaybeT m b'.
liftToMaybeT :: Monad m => m (Maybe b) -> MaybeT m b
liftToMaybeT = lift >=> liftMaybe

-- | Execute a monad action on the contents of a 'Maybe a' if it contains a value; otherwise do
-- nothing. This is just an alias to make it more clear what's actually happening, since folding
-- a 'Maybe' isn't very intuitive (to me) --- it should get inlined by GHC.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

-- | Map one of two possible functions over an Either, depending on which variant it contains.
-- The first function is used if it is a Left, and the second if it is a Right.
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left a) = Left $ f a
mapBoth _ f (Right b) = Right $ f b
