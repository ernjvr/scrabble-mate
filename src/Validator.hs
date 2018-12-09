module Validator
    ( isWordInDictionary
    , isAllCharsValid
    ) where

import Factory
import Util
import qualified Data.Map as M
import Data.Maybe

isWordInDictionary :: Foldable t => [Char] -> t [Char] -> Bool
isWordInDictionary word dictionary =
  toLowerCase word `elem` dictionary

isAllCharsValid :: [Char] -> String -> Bool
isAllCharsValid [] _ = False
isAllCharsValid word randomString =
  let chars = toLowerCase word
      string = toLowerCase randomString
      allCharsInString = isAllCharsInString chars string
      charsCountValid = isCharCountValid chars string
  in allCharsInString && charsCountValid

isAllCharsInString :: (Foldable t, Eq a) => [a] -> t a -> Bool
isAllCharsInString chars string = not (length [ x | x <- chars, not (x `elem` string) ] > 0)

isCharCountValid :: Ord a2 => [a2] -> [a2] -> Bool
isCharCountValid word randomString =
  let wordMap = makeMap word
      randomMap = makeMap randomString
  in not (isCharCountExceeded wordMap randomMap)

isCharCountExceeded :: (Ord a, Ord a2) => M.Map a2 a -> M.Map a2 a -> Bool
isCharCountExceeded wordMap randomMap =
  let wordKeys = M.keys wordMap
  in isCharCountGreater wordKeys wordMap randomMap

isCharCountGreater :: (Ord a1, Ord a2) => [a2] -> M.Map a2 a1 -> M.Map a2 a1 -> Bool
isCharCountGreater [] _ _ = False
isCharCountGreater (c:cs) wordMap randomMap =
  let wordCharCount = fromJust (M.lookup c wordMap)
      randomCharCount = fromJust (M.lookup c randomMap)
  in if wordCharCount > randomCharCount then True else isCharCountGreater cs wordMap randomMap