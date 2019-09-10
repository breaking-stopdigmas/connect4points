module Main where

import System.Exit

import Graphics.Gloss hiding (color)
import Graphics.Gloss.Interface.IO.Game hiding (color)

import Board hiding (Color)
import GraphicsBoard (drawBoard)
import Player
import AI (GamePosition)

-- função que captura os movimentos dos players
moveFunc :: (Player Column Board) -> (Player Column Board) -> Event -> Board -> IO Board
moveFunc player1 player2 (EventKey (MouseButton LeftButton) Up _ (coordX, _)) b@Board{ winner = Empty } = let col = ceiling $ (coordX + 350) / 100
                                                                                                              moves = possibleMoves b
                                                                                                          in if null moves
                                                                                                             then return b { winner = Both } 
                                                                                                             else moveFunc' moves player1 player2 b col
moveFunc _ _ (EventKey (MouseButton LeftButton) Up _ _) _ = exitSuccess
moveFunc _ _ (EventKey (SpecialKey KeyEsc) Up _ _) _      = die "Game Aborted"
moveFunc _ _ (EventKey (Char 'q') Up _ _) _               = die "Game Aborted"
moveFunc _ _ _ b                                          = return b

moveFunc' moves player1 player2 b col =
    do
        let player = if (color b == Red) then player1 else player2
        move <- player moves b (Just col)
        let nBoard = makeMove b move
            hasWon = checkWin nBoard
        if hasWon
        then return nBoard { winner = color b }
        else return nBoard

timeFunc :: Float -> Board -> IO Board
timeFunc _ b = return b

-- define as dimensões da tela e sua grade de divisão
window :: Display
window = InWindow "Connect 4 !!" (700, 600) (10, 10)

-- define a cor do fundo da tela
background :: Color
background = dark blue

-- Função para escolhar do tipo dos players(1 e 2).
choosePlayer :: GamePosition b => Int -> IO (Player a b)
choosePlayer i = do putStrLn $ "Choose Player " ++ (show i) ++ ":" -- indica qual player está sendo selecionado
                    putStrLn "1. Human"
                    putStrLn "2. Computer"
                    putStrLn "Enter Choice: "
                    choiceString <- getLine -- pega o valor digitado no teclado
                    let choice = read choiceString -- choice recebe a leitura da choiceString
                    case choice of -- tratamento de qual o retorno será da função em detrimento do valor de entrada
                      1 -> return ioplayer
                      2 -> return randplayer
                      _ -> do putStrLn $ "You have entered an invalid choice: " ++ (show choice)
                              putStrLn "Please choose one of the possible choices."
                              choosePlayer i

begin :: IO ()
begin = do
  -- função para seleção do jogador
  player1 <- choosePlayer 1
  player2 <- choosePlayer 2
  -- Função da Graphics.Gloss para montar a tela e que responde às mudanças conforme as entradas que recebe
  playIO window background 1 initialBoard drawBoard (moveFunc player1 player2) timeFunc

-- A função main recebe uma ação de I/O é alguma coisa que, quando executada, 
-- irá realizar uma ação com um efeito colateral (que é usualmente ler do 
-- dispositivo de entrada ou imprimir algo na tela) e irá também conter algum 
-- tipo de valor de retorno dentro dela
main :: IO ()
main = begin
