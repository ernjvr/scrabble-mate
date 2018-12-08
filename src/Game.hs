module Game
    ( score
    , getScore
    , isGameComplete
    , displayTurnCounter
    , displayTurnResult
    , displayScore
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
        putStrLn $ "Congratulations! You entered a valid word"
      else
        putStrLn $ "Sorry! You entered an invalid word."

displayScore :: Int -> IO()
displayScore score = putStrLn $ "Score: " ++ show score