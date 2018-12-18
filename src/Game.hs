module Game
    ( score
    , getScore
    , isGameComplete
    , displayTurnCounter
    , displayTurnResult
    , displayScore
    , displayRandomString
    , displayUserPrompt
    , displayCharsValidity
    , displayGameComplete
    ) where

import System.Random

score :: String -> Int -> Int
score word previousScore  = length word + previousScore

getScore :: Bool -> String -> Int -> Int
getScore result word scoreboard =
               if result
                 then
                   score word scoreboard
                 else
                   scoreboard

isGameComplete :: Int -> Bool
isGameComplete round = round == 10

displayTurnCounter :: Int -> IO()
displayTurnCounter round = putStrLn $ "Round " ++ show round ++ "/" ++ show 10

displayTurnResult :: Bool -> IO()
displayTurnResult result =
    if result
      then
        putStrLn $ "Congratulations! You entered a valid word."
      else
        putStrLn $ "Sorry! You entered an invalid word."

displayScore :: Int -> IO()
displayScore scoreboard = putStrLn $ "Score: " ++ show scoreboard

displayRandomString :: String -> IO()
displayRandomString randomString = putStrLn $ "Random letters: " ++ show randomString

displayUserPrompt :: IO()
displayUserPrompt = putStr "Please enter a word using a combination of the letters above: "

displayCharsValidity :: Bool -> IO()
displayCharsValidity validChars =
  if validChars then
      putStrLn $ "All characters entered are valid :)"
    else
      putStrLn $ "Sorry! The characters entered are invalid. Please only use characters from the random letters above. Do not use more of the same type of characters than what is provided above."

displayGameComplete :: Int ->  IO()
displayGameComplete scoreboard = putStrLn $ "Congratulations! You completed the game.\nFinal Score: " ++ show scoreboard