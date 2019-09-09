module AI where

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int
