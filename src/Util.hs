module Util where

(=>>) :: Monad m => m b -> (b -> m a) -> m b
a =>> f = do
  val <- a
  f val
  return val

justOr :: a -> Maybe a -> a
justOr _ (Just x) = x
justOr defaultVal Nothing = defaultVal
