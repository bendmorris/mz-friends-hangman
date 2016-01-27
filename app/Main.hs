module Main where

import Data.Set (Set)
import Data.Char
import Data.List
import System.IO

import qualified Data.Set as S

data HangmanGame = HangmanGame
    { gameWord    :: String
    , gameGuessed :: [Char]
    }

main :: IO ()
main = do putStrLn "Welcome to hangman!"
          hSetBuffering stdin NoBuffering
          let game = HangmanGame {gameWord = "rabbit", gameGuessed = []}
          putStrLn $ gallows 0
          putStrLn $ blanks game
          playGame game

playGame :: HangmanGame -> IO ()
playGame game = do letter <- getChar
                   let newGame = guessLetter (toLower letter) game
                   putStrLn $ gallows $ misses newGame
                   putStrLn $ blanks newGame
                   putStrLn $ intercalate " " [[c] | c <- reverse $ gameGuessed newGame, not (elem c (gameWord game))]
                   case (gameStatus newGame) of
                     Won -> putStrLn "You won!"
                     Lost -> putStrLn ("You lost...the word was " ++ (gameWord game))
                     InProgress -> playGame newGame

totalBodyParts :: Int
totalBodyParts = 6

gallows :: Int -> String
gallows n = "\n\
\    __________\n\
\    |        |\n\
\    |        " ++ (if n >= 1 then "0" else " ") ++ "\n\
\    |       " ++ (if n >= 3 then "/" else " ") ++ (if n >= 2 then "|" else " ") ++ (if n >= 4 then "\\" else " ") ++ "\n\
\    |       " ++ (if n >= 5 then "/" else " ") ++ " " ++ (if n >= 6 then "\\" else " ") ++ "\n\
\    |\n\
\    |\n\
\---------"

blanks game = intercalate " " [if elem c (gameGuessed game) then [c] else "_" | c <- gameWord game]

guessLetter :: Char -> HangmanGame -> HangmanGame
guessLetter c game
    | ((gameStatus game == Won) || (gameStatus game == Lost)) = game
    | otherwise = game { gameGuessed = c : gameGuessed game }

data GameStatus = Won | Lost | InProgress deriving Eq

gameStatus :: HangmanGame -> GameStatus
gameStatus game =
    if eachCharIsIn (gameWord game) (gameGuessed game)
        then Won
        else if (misses game) >= totalBodyParts then Lost else InProgress

eachCharIsIn :: String -> [Char] -> Bool
eachCharIsIn s c = all (\a -> elem a c) s

misses :: HangmanGame -> Int
misses game = numWrongGuesses (gameWord game) (gameGuessed game)

numWrongGuesses :: String -> [Char] -> Int
numWrongGuesses word cs = go (S.fromList word) cs
  where
    go :: Set Char -> [Char] -> Int
    go wordChars [] = 0
    go wordChars (c:cs)
        | c `S.member` wordChars = go wordChars cs
        | otherwise = 1 + go wordChars cs

isFinished :: HangmanGame -> Bool
isFinished game = case gameStatus game of
    InProgress -> False
    otherwise -> True
