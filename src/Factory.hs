module Factory
      ( makeRandomString
      , makeMap
      )
      where

import System.Random
import qualified Data.Map as M

makeRandomString :: RandomGen rg => rg -> String
makeRandomString randomGenerator =
  let (gen1, gen2) = split randomGenerator
  in take 20 $ randomRs ('a', 'z') gen1

incrementCharCount :: (Ord k, Num a) => k -> M.Map k a -> M.Map k a
incrementCharCount key map = M.adjust (1 +) key map

makeMap :: (Ord a1, Num a2) => [a1] -> M.Map a1 a2
makeMap chars =
  let initializedMap = initializeMap chars
  in updateCharCountsOfMap chars initializedMap

initializeMap :: (Ord k, Num a) => [k] -> M.Map k a
initializeMap word =
  let tuplify char = (char, 0)
      list = map tuplify word
  in M.fromList list

updateCharCountsOfMap :: (Ord a1, Num a2) => [a1] -> M.Map a1 a2 -> M.Map a1 a2
updateCharCountsOfMap [] map = map
updateCharCountsOfMap (c:cs) map =
  let newMap = incrementCharCount c map
  in if length cs > 0 then updateCharCountsOfMap cs newMap else newMap
