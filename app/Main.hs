module Main where

import Factory
import Game
import Validator
import Data.Char
import System.Random
import Data.List
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
  fileContent <- readFile "dictionary.txt"
  let dictionary = words (map toLower fileContent)
      scoreboard = 0
      round = 1
  playTurn dictionary scoreboard round

playTurn :: Foldable t => t [Char] -> Int -> Int -> IO ()
playTurn dictionary scoreboard round = do
  displayTurnCounter round
  randomGenerator <- newStdGen
  let randomString = makeRandomString randomGenerator
  putStrLn $ randomString
  putStr "Please enter a word using a combination of the letters above "
  wordFromUser <- getLine
  let word = toLowerCase wordFromUser
      validChars = isAllCharsValid word randomString
  if validChars then
    putStrLn $ "All characters entered are valid :)"
  else
    putStrLn $ "Sorry! The characters entered are invalid. Please only use characters from the random letters above. Do not to use more of the same type of characters than what is provided above."
  let wordInDictionary = isWordInDictionary word dictionary
      result = validChars && wordInDictionary
      newScore = getScore result word scoreboard
  displayTurnResult result
  displayScore newScore
  if isGameComplete round
    then
      putStrLn $ "Congratulations! You completed the game.\n Final Score: " ++ show newScore
    else
      playTurn dictionary newScore (round + 1)