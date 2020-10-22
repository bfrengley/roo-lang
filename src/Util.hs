module Util where

(=>>) :: Monad m => m b -> (b -> m a) -> m b
b =>> f = b >>= f >> b
