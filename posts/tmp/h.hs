import Prelude hiding (Functor, fmap)
import Data.Set as Set
class Functor f where
  fmap :: (a -> b) -> f a -> f b

data State = Empty | Full deriving Show
data Func2 a = F2 a State deriving Show

instance Functor Func2 where
  fmap f (F2 a _) = F2 (f a) Full

class Functor' f where
  fmap' :: Ord b => (a -> b) -> f a -> f b

data Func'3 a = F'3 (Set.Set a) deriving Show

instance Functor' Func'3 where
  fmap' f (F'3 s) = F'3 (Set.map f s)
  
{-
data FuncB a = Fb [a] deriving Show

instance Functor FuncB where
  fmap f (Fb (x:xs)) = Fb (ys ++ [y])
    where Fb ys = fmap f (Fb xs)
          y = f x
  fmap f (Fb []) = Fb []

-}
