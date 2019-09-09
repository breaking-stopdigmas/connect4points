{-# LANGUAGE InstanceSigs #-}

module Board (
  board,
  color,
  winner,
  rows,
  columns,
  Color (Empty, Red, Yellow, Both),
  Board (Board),
  Column,
  initialBoard,
  possibleMoves,
  makeMove,
  checkWin,
  valuation,
  opp
  ) where

import Data.List (intercalate, transpose)
import qualified Data.Map.Strict as Map (insert, fromList)
import Data.Map.Strict((!), Map)

import AI

rows = 6
columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- definindo 3 possibilidades de cores
data Color = Empty | Red | Yellow | Both deriving (Eq, Show)

-- definindo estrutura da Board (tela)
data Board = Board { board :: (Map Coords Color)
                   , heights :: (Map Column Row)
                   , color :: Color
                   , winner :: Color
                   }

-- função que constroi board  
initialBoard  = Board { board = (Map.fromList [ ((x, y), Empty) | x <- [1..columns], y <- [1..rows]])
                      , heights = (Map.fromList [ (col, 0) | col <- [1..columns]])
                      , color = Red
                      , winner = Empty
                      }

-- Retorna colunas cuja linha superior ainda não foi preenchida
possibleMoves :: Board -> [Column]
possibleMoves b
  | checkWin b = []
  | otherwise = [x | x <- [1..columns], (heights' ! x) /= rows]
                where
                  heights' = heights b

-- Atualiza a Board
makeMove :: Board -> Column -> Board
makeMove b@Board{ heights = heights', color = c, board = board' } col =
  let posMoves = possibleMoves b
      curHeight = heights' ! col
      nBoard = Map.insert (col, curHeight + 1) c board'
      nHeights = Map.insert col (curHeight + 1) heights'
  in if (col `elem` posMoves)
     then b { board = nBoard, heights = nHeights, color = opp c }
     else b

opp :: Color -> Color
opp c
  | c == Red = Yellow
  | c == Yellow = Red

-- captura círculos selecionados
verticals :: Board -> [[Color]]
verticals b = [ [board b ! (x, y + i) | i <- [0..3]] | x <- [1..columns], y <- [1..(rows - 3)]]

horizontals :: Board -> [[Color]]
horizontals b = [ [board b ! (x + i, y) | i <- [0..3]] | x <- [1..(columns - 3)], y <- [1..rows]]

rdiags :: Board -> [[Color]]
rdiags b = [ [board b ! (x + i, y + i) | i <- [0..3]] | x <- [1..(columns - 3)], y <- [1..(rows - 3)]]

ldiags :: Board -> [[Color]]
ldiags b = [ [board b ! (x + i, y - i) | i <- [0..3]] | x <- [1..(columns - 3)], y <- [4..rows]]

-- Verifique se um jogador ganhou
checkWin :: Board -> Bool
checkWin board = or [f board | f <- [checkWinCol, checkWinRow, checkWinDiagRight, checkWinDiagLeft]]

-- verifica se existe algum quad, de modo que todos os elementos de um quad sejam da cor oposta.
-- Se sim, o outro jogador venceu e atingiu este estado
checkWin' :: (Board -> [[Color]]) -> Board -> Bool
checkWin' f b = or $ map (and . map ((==) (opp $ color b))) $ f b

checkWinCol :: Board -> Bool
checkWinCol = checkWin' verticals

checkWinRow :: Board -> Bool
checkWinRow = checkWin' horizontals

checkWinDiagRight :: Board -> Bool
checkWinDiagRight = checkWin' rdiags

checkWinDiagLeft :: Board -> Bool
checkWinDiagLeft = checkWin' ldiags

-- Avalia se houve alguma sequencia dentro da 4 possibilidades
valuation :: Board -> Int
valuation board = sum [f board | f <- [valuationCol, valuationRow, valuationDiagRight, valuationDiagLeft]]

-- Calcula a pontuação de cada conjunto de quad e retornar a soma
valuation' :: (Board -> [[Color]]) -> Board -> Int
valuation' f b = sum $ map (scoreQuad $ color b) $ f b

valuationCol :: Board -> Int
valuationCol = valuation' verticals

valuationRow :: Board -> Int
valuationRow = valuation' horizontals

valuationDiagRight :: Board -> Int
valuationDiagRight = valuation' rdiags

valuationDiagLeft :: Board -> Int
valuationDiagLeft = valuation' ldiags

scoreQuad :: Color -> [Color] -> Int
scoreQuad color colors = if (and [ c == Empty || c == color | c <- colors])
                         then case (length $ filter (== color) colors) of 3 -> 1
                                                                          _ -> 0
                         else 0

instance GamePosition Board where
  moves :: Board -> [Board]
  moves b = map (makeMove b) $ possibleMoves b

  static :: Board -> Int
  static b = if (checkWin b) then -100 else (valuation b) - (valuation b { color = opp (color b) })

instance Show Board where
  show :: Board -> String
  show board' = let b = board board'
                    strBoard = [[if z == Red then "R" else if z == Yellow then "Y" else " " | x <- [1..columns], let z = b ! (x, y)] | y <- [rows,(rows-1)..1]]
                    rowSep = "\n" ++ (concat $ replicate columns "+---") ++ "+\n"
                    output = intercalate rowSep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strBoard
                in rowSep ++ output ++ rowSep
