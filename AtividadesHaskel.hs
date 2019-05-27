module Atividades where

-- double recebe um inteiro e retorna o seu valor duas vezes maior
double :: Int -> Int
double k = 2 * k

--double2 recebe um inteiro e retorna o seu valor 4 vezes maior
double2 :: Int -> Int
double2 k = 2 * double k

-- sel atua da seguinte forma:
-- -- Caso o primeiro parametro é True  retorna o segundo  parametro
-- -- Caso o primeiro parametro é False retorna o terceiro parametro
sel :: Bool -> Int -> Int -> Int
sel a b c
    |a = b
    |otherwise = c

-- max2 retorn o maior valor entre dos dois valores
-- Utilize sel para resolver esse problema
max2 :: Int -> Int -> Int
max2 = undefined

-- max2' retorn o maior valor entre dos dois valores
-- Não sel para resolver esse problema
max2' :: Int -> Int -> Int
max2' = undefined

-- max3 retorn o maior valor entre dos três valores
-- Utilize max2 ou max2' para resolver esse problema
max3 :: Int -> Int -> Int -> Int
max3 = undefined

-- m recebe um inteiro e, caso seja maior que 100, retorna n-10,
-- caso contrário então retorna m (m (n+11))
m :: Int -> Int
m = undefined
-- Qual valor esta função retorna para valores de entrada positivos
-- menores que 101?

-- num2digits que recebe um inteiro e transforma em uma
-- lista de dígitos correspondente ao número
-- -- num2digits 4721 => [4,7,2,1]
num2digits :: Int -> [Int]
--num2digits n = map (mod 10) . reverse . takeWhile ( > 0 )

-- Um número é dito chic se o digito resultante da soma de seus dígitos
-- ocorre no número. Se o resultado da soma dos dígitos for um número
-- com mais de um dígito, então o processo deve ser repetido até
-- que se obtenha um único dígito.
-- -- 1276 é chic, pois 1+2+7+6 = 16, 1+6 = 7.
-- -- 123 não é chic, 1+2+3 = 6
chic :: Int -> Bool
--chic x = elem s num
--    where
--        num = num2digits x
--        s = sumLoop x
--        sumLoop :: Int -> Int
--        sumLoop x
--            |x mod 10 == 0 = x
--            |otherwise = sumLoop $ sum $ num2digits x

-----------------------------------------------------------------------------------------
-- Tipos de dados - Intro
-----------------------------------------------------------------------------------------

-- É possível implementar as frações como duplas como, por exemplo,
-- 3/5 = (3,5)
-- 2/8 = (2,8)
-- 3/4 = (3,4)

-- -- multf que recebe duas frações e faz a sua multiplicação
multf :: (Int, Int) -> (Int, Int) -> (Int, Int)
multf = undefined

-- -- somaf que recebe duas frações e faz a sua soma
somaf :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaf = undefined

-- -- subf que recebe duas frações e faz a sua subtração
subf :: (Int, Int) -> (Int, Int) -> (Int, Int)
subf = undefined


-- -- divf que recebe duas frações e faz a sua divisão
divf :: (Int, Int) -> (Int, Int) -> (Int, Int)
divf = undefined

-- -- toReal que tranforma a fração em um valor real
toReal :: (Int, Int) -> Float
toReal = undefined
