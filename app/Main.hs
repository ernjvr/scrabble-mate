module Main where

import Factory
import Game
import Validator
import Util
import Data.Char
import System.Random
import System.IO
import Data.List
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
  fileContent <- readFile "dictionary.txt"
  let dictionary = words (toLowerCase fileContent)
      scoreboard = 0
      roundNumber = 1
  hSetBuffering stdout NoBuffering
  playTurn dictionary scoreboard roundNumber

playTurn :: Foldable t => t [Char] -> Int -> Int -> IO ()
playTurn dictionary scoreboard roundNumber = do
  displayScore scoreboard
  displayTurnCounter roundNumber
  randomGenerator <- newStdGen
  let randomString = makeRandomString randomGenerator
  displayRandomString randomString
  displayUserPrompt
  wordFromUser <- getLine
  let word = toLowerCase wordFromUser
      validChars = isAllCharsValid word randomString
  displayCharsValidity validChars
  let validWordInDictionary = isWordInDictionary word dictionary
      turnResult = validChars && validWordInDictionary
      newScoreboard = getScore turnResult word scoreboard
  displayTurnResult turnResult
  if isGameComplete roundNumber
    then
      displayGameComplete newScoreboard
    else
      playTurn dictionary newScoreboard (roundNumber + 1)