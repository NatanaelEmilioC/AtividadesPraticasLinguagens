--Questão 01 lista 04
andTern :: Int -> Int -> Int
andTern a b
    | a == 1 = b
    | a == 2 , b /= 0 = a
    | otherwise = 0

orTern :: Int -> Int -> Int
orTern a b
    | a == 1 || b == 1 = 1
    | a == b = a
    | otherwise = 2

notTern :: Int -> Int
notTern b
    | b == 2 = 2
    | otherwise = (b + 1) `mod` 2

xorTern :: Int -> Int -> Int
xorTern a b
    | a == notTern b = 1
    | a /= notTern b = 0
    | otherwise = 2