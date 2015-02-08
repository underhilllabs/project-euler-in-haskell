import Data.Char
import Data.List
import System.IO

main = do
  handle <- openFile "names.txt" ReadMode
  contents <- hGetContents handle
  let names = sort $ words contents
  let total = sum $ zipWith (\word n -> n * (wordsum word)) names [1..]
  print total
      
wordsum :: String -> Int
wordsum [] = 0
wordsum (x:xs) = ord(x) - 64 + wordsum xs
