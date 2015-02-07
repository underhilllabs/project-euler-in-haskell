import Data.Char
import Data.List
import System.IO

main = do
  handle <- openFile "names.txt" ReadMode
  contents <- hGetContents handle
  let names = sort $ words contents
  --print $ length $ sort names
  --print $ wordsum "BOSSY"
  --print $ wordsum "SEAMUS"
  let count = 1
  let totals = zipWith (\w n -> n * (wordsum w)) names [1..]
  let total = sum totals
  print total
      
wordsum :: String -> Int
wordsum [] = 0
wordsum (x:xs) = ord(x) - 64 + wordsum xs
