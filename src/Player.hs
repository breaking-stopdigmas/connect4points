module Player where

import System.Random

import Util (randomPick)
import AI (GamePosition)

type Player a b = [a] -> b -> Maybe a -> IO a

ioplayer :: GamePosition b => Player a b
ioplayer moves b (Just c) = return c
ioplayer moves b Nothing = error "i need an IO entry to work"

randplayer :: Player a b
randplayer moves _ _ = getStdRandom (randomPick moves)

