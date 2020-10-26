module Util where

-- | The 'Equivalent' class defines the concept of "loose" equality, as opposed to the strict
-- mathematical equality defined by 'Eq'. This is primarily useful where you may want to define a
-- looser concept of equality for a type while still being able to use 'Eq' (such as for types
-- which may be keys of a map). An example case where loose equality is useful is a bottom type,
-- which, while mathematically it should be equal to other types, probably shouldn't be treated as
-- the same key in a map.
class Equivalent a where
  (=%=) :: a -> a -> Bool

(=>>) :: Monad m => m b -> (b -> m a) -> m b
a =>> f = do
  val <- a
  f val
  return val
