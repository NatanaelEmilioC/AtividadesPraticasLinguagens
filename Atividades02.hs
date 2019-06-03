--Questão 01 lista 04
andTerm :: Int -> Int -> Int
andTerm a b
    | a == 1 = b
    | a == 2 , b /= 0 = a
    | otherwise = 0

orTerm :: Int -> Int -> Int
orTerm a b
    | a == 1 || b == 1 = 1
    | a == b = a
    | otherwise = 2

notTerm :: Int -> Int
notTerm b
    | b == 2 = 2
    | otherwise = (b + 1) `mod` 2

xorTerm :: Int -> Int -> Int
xorTerm a b = andTerm (notTerm $ andTerm a b) $ orTerm a b  

gcd1 :: Int -> Int -> Int
gcd1 a b
    | b == 0 = a
    | otherwise = gcd1 b $ a `mod` b

lcm1 :: Int -> Int -> Int
lcm1 a b = div (a*b) $ gcd1 a b

cosA :: Double -> Double -> Double
cosA a b = undefined