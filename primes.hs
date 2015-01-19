divides :: Int -> Int -> Bool
divides d n = rem n d == 0

ld :: Int -> Int
ld n = ldf 2 n

ldf :: Int -> Int -> Int
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n
