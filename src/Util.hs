module Util(
        toLowerCase
      ) where

import Data.Char

toLowerCase :: String -> String
toLowerCase word = (map toLower word)

