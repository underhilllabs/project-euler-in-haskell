import System.IO
import Data.List

main = do
  contents <- readFile "8.txt"
  let num = removeNewlines contents
  let thenum = read(num) :: Integer
  let nums = multNDigits $ show thenum
  let sorted_nums = sort (nums)
  let max = head $ reverse $ sorted_nums
  print max
  
removeNewlines :: String -> String
removeNewlines [] = ""
removeNewlines (x:xs) | x == '\n' = removeNewlines xs
                      | otherwise = x : removeNewlines xs

multDigits :: [Char] -> Integer
multDigits [] = 1
multDigits xs = (read (take 1 xs) :: Integer) * multDigits(tail xs)

multNDigits :: [Char] -> [Integer]
multNDigits [] = []
multNDigits xs | length(take 13 xs) < 13 = []
                    | otherwise = (multDigits $ take 13 xs) : (multNDigits $ tail xs) 
