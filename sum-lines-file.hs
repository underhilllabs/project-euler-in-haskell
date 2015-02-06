import System.IO
import Data.List

main = do
  handle <- openFile "13.txt" ReadMode
  contents <- hGetContents handle
  let numStrs = lines contents
  let nums = map (\x -> read(x) :: Integer) numStrs
  print $ take 10 $ show $ sum nums 
  

