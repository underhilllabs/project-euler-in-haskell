divides :: Int -> Int -> Bool
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

fibNext :: Int -> Int -> Int
fibNext x y = x + y

-- [ fizbuzz x | x <- [1..100] ]
fizbuzz :: Int -> String
fizbuzz n | rem n 3 == 0 && rem n 5 == 0 = "fizbuzz"
          | rem n 3 == 0 = "fiz"
          | rem n 5 == 0 = "buzz"
          | otherwise = show n

-- fizUpTo 100
fizUpTo :: Int -> [String]
fizUpTo n = [fizbuzz x | x <- [1..n]]

