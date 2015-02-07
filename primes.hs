
divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

ld :: Int -> Int
ld n = ldf 2 n

ldf :: Int -> Int -> Int
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

prime0 :: Int -> Bool
prime0 n = ld n == n

-- sum (mult3or5range 999)
mult3or5range :: Int -> [Int]
mult3or5range n = [x | x <- [1..n], mult3or5 x]

mult3or5 :: Int -> Bool
mult3or5 n  | rem n 3 == 0 = True
            | rem n 5 == 0 = True
            | otherwise = False

--fibUpTo n :: Int -> [Int]
-- fibUpTo n = [x | x <- fibNext 0 1,   

--fibNext :: Int -> Int -> Int
--fibNext x y = x + y

-- [ fizbuzz x | x <- [1..100] ]
fizbuzz :: Int -> String
fizbuzz n | rem n 3 == 0 && rem n 5 == 0 = "fizbuzz"
          | rem n 3 == 0 = "fiz"
          | rem n 5 == 0 = "buzz"
          | otherwise = show n

-- fizUpTo 100
fizUpTo :: Int -> [String]
fizUpTo n = [fizbuzz x | x <- [1..n]]

factors :: Int -> [Int]
factors n = [x | x <- [2..n], divides x n]

factors2 :: Integral a => a -> [a]
factors2 n = n:[x | x <- [1..n `div` 2], divides x n]

divisors :: Int -> [Int]
divisors n = [ x | x <- [1..(n-1)], divides x n]

-- This one includes 1.
divisors2 :: Int -> [Int]
divisors2 n = [x | x <- [1..n], divides x n]

sumDiv :: Int -> Int
sumDiv n = sum (divisors n)

amicableNum :: Int -> Bool
amicableNum n = sumDiv(sumDiv(n)) == n && (sumDiv(n) /= n)   

amicableNums :: Int -> [Int]
amicableNums n = [ x | x <- [1..n], amicableNum x ]

primeFactors :: Int -> [Int]
primeFactors n = [x | x <- (factors n), prime0 x]

-- take 6 primeList
primeList :: [Int]
primeList = [x | x <- [2..], prime0 x]

fibNext :: [Int] -> Int
fibNext [] = 0
fibNext [0] = 1
fibNext (xs) =  sum (take 2 (reverse xs))

fibList :: [Int] -> [Int]
fibList (xs) = xs ++ [fibNext xs]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2) 

-- generate Fibonacci numbers 
fibs :: [Int]
fibs = [fib x | x <- [0..]]

-- do all the numbers in xs divide even into n
dividesArr :: Int -> [Int] -> Bool
dividesArr n [] = True
dividesArr n (x:xs) = divides x n && dividesArr n xs

sumOfSq :: Int -> Int
sumOfSq n = sum([ x^2 | x <- [1..n] ])

sqOfSums :: Int -> Int
sqOfSums n = sum( [ x | x <- [1..n] ] )^2 

isPalindrome :: String -> Bool
isPalindrome (xs) = xs == reverse(xs)

-- :m + Data.List
-- sort [ x*y | x <- [10..99], y <- [10..99], isPalindrome( show( x*y )) ]

pyTriple :: Int -> Int -> Int -> Bool
pyTriple a b c = (a^2 + b^2) == c^2

-- [a*b*c | c <- [3..1000], b <- [2..(c-1)], a <- [1..(b-1)], pyTriple a b c, a + b + c == 1000]

--head show (2^1000) 

sumDigits :: [Char] -> Int
sumDigits [] = 0
sumDigits xs = (read (take 1 xs) :: Int) + sumDigits(tail xs)

multDigits :: [Char] -> Integer
multDigits [] = 1
multDigits xs = (read(take 1 xs) :: Integer) * multDigits(tail xs)

sumDigs :: Int -> Int
sumDigs n = (read(take 1 (show n)) :: Int) + sumDigits(tail (show n))

numStr :: Int -> Int -> [Char]
numStr x max | x > max = ""
             | otherwise = show (x) ++ numStr (x+1) max 

fact :: Int -> Int 
fact 0 = 1 
fact n = product [1..n]
 
-- return the sum of the factorials of the digits
factDigs :: [Char] -> Int 
factDigs [] = 0 
factDigs xs = fact(read (take 1 xs) :: Int) + factDigs(tail xs) 

-- length(nub (powersList 2 100) )
powersLst :: Int -> Int -> [Int]
powersLst a b = [ a^b | a <- [a..b], b <- [a..b]]

fifthDigs :: [Char] -> Int
fifthDigs [] = 0
fifthDigs xs = (read(take 1 xs):: Int)^5 + fifthDigs(tail xs)

fifthDigP :: Int -> Bool
fifthDigP n = n == fifthDigs (show n)

fibNxt :: Int -> Int -> [Int]
fibNxt 0 0 = 1 : fibNxt 0 1
fibNxt x y = (x+y) : fibNxt y (x+y)

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger(round x)

-- sumDigsPow :: Int -> Bool
-- sumDigsPow x = isInt(logBase sumDigs(x)  x)

triNum :: Int -> Int
triNum n = n * (n+1) `div` 2

triangle1 = scanl (+) 1 [2..]

removeNewlines :: String -> String
removeNewlines [] = ""
removeNewlines (x:xs) | x == '\n' = removeNewlines xs
                      | otherwise = x : removeNewlines xs


rightTri :: Integer -> [(Integer, Integer, Integer)]
rightTri n = [(x, y, z)| z <- [1..(n `div` 2)], y <- [1..z], x <- [1..z], x^2 + y^2 == z^2, x+y+z == n] 

-- [(n, len)| n <- [1..1000], let len = length $ rightTri n, len > 2]
