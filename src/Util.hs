module Util where

import System.Random

-- Função que irá gerar uma posição aleatória para o player do computador
randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick xs gen = let (index, gen') = randomR (0, (length xs) - 1) gen
                    in (xs !! index, gen')
