-- this how you comment code in Haskell
divides d n = rem n d == 0

-- the equal sign is meant to be the function definition
ld :: Integer -> Integer
ld n = ldf 2 n

-- a primality test. Brute force... compact so compact
ldf k n | divides k n = k              -- found! no prime this time
        | k^2 > n     = n              -- too high.. got a prime
        | otherwise   = ldf (k + 1) n  -- look again.

prime0 n | n < 1     = error "not a positive integer" -- compact errors
         | n == 1    = False
         | otherwise = ld n == n

{-
multiline
comment
-}

{-
type declaring can be either specified or inferred.
-}

-- ex 1.9
min' :: Int -> Int -> Int
min' a b | a <= b     = a
         | otherwise  = b

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min' x (mnmInt xs)

max' :: Int -> Int -> Int
max' x y | x > y      = x
         | otherwise  = y

maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max' x (maxInt xs)

removeFst :: Eq a => a -> [a] -> [a]
removeFst x [] = error "empty list"
removeFst x [y] | [x] == [y] = []
                | otherwise  = [y]
removeFst x (y:ys) | x == y = ys
                   | otherwise = y : removeFst x ys

average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- exercise 1.13
count :: Char -> String -> Int
count x [] = 0
count x (y:ys) | x == y    = 1 + count x ys
               | otherwise = count x ys

blowup :: String -> String
blowup [] = []
blowup (c:xs) = [c] ++ [c] ++ [c] ++ blowup xs

srtInts :: [Int] -> [Int]
srtInts [] = [] -- empty list in empty list out
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

-- exercise 1.15
minStr :: String -> String -> String
minStr s1 s2 | s1 <= s2  = s1
             | otherwise = s2
mnmStr :: [String] -> String
mnmStr [] = error "empty list"
mnmStr [s] = s
mnmStr (x:xs) = minStr x (mnmStr xs)

srtStrs :: [String] -> [String]
srtStrs []     = [] -- the empty string!
srtStrs     xs = m : (srtStrs (removeFst m xs)) where m = mnmStr xs

-- example 1.16
prefix :: String -> String -> Bool
prefix [] ys         = True
prefix xs []         = False
prefix (x:xs) (y:ys) | x == y = prefix xs ys
                     | x /= y = False

prefix_sol :: String -> String -> Bool
prefix_sol [] ys = True
prefix_sol (x:xs) [] = False
prefix_sol (x:xs) (y:ys) = (x==y) && prefix xs ys -- nesting deeper &&-chain

-- exercise 1.17
substring :: String -> String -> Bool
substring [] ys     = True
substring xs []     = False
substring xs (y:ys) = prefix xs (y:ys) || substring xs ys

substring_sol :: String -> String -> Bool
substring_sol [] ys = True
substring_sol (x:xs) [] = False
substring_sol (x:xs) (y:ys) = ((x==y) && (prefix xs ys)) || (substring (x:xs) ys)

--
