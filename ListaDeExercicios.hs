import Data.Bits

--1)
repetir :: a -> [a]
repetir a = as where as = a:as


--2
--a)
data Bit = Zero | Um deriving (Show, Eq)

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit x
    |x `mod` 2 == 1 = int2bit (x `div` 2) ++ [Um]
    |otherwise = int2bit (x `div` 2) ++ [Zero] 


--b)
bit2int :: [Bit] -> Int
bit2int [] = 0
bit2int (x:xs)
    | x == Zero = (0 * 2 ^(length xs)) + bit2int xs
    | otherwise = (1 * 2 ^(length xs)) + bit2int xs

--3 Crie um tipo de dados que represente uma fórmula, sendo uma fórmula sendo:
--Verdadeiro
--Falso
--E, necessário duas fórmulas
--Ou, necessário duas fórmulas
--Não necessário uma fórmula
--Dado que foi criado o tipo de dado Fórmula, dena a função eval :: Formula -> Bool que recebevuma fórmula e a avalia.

data Formula = V
                | F
                | E Formula Formula
                | Nao Formula
                | Ou Formula Formula 
            deriving Show 

eval :: Formula -> Bool
eval (V) = True
eval (F) = False
eval (E p q) = eval p && eval q
eval (Ou p q) = eval p || eval q
eval (Nao q) = not(eval q)

--4 Utilizando somente compressão de lista - List Comprehension - dena as seguintes funções:
--a)que recebe uma lista e retorna um lista somente com números inteiros.
allEven :: [Int] -> [Int]
allEven xs = [ x | x <- xs, x `mod` 2 == 0 ]

--b) recebe uma lista de listas e retorna com o tamanho desse lista e a lista correspondente.
lengths :: [[a]] -> [(Int, [a])]
lengths xs = [( y , x ) | x <- xs , let y = length x ]

--c) que recebe duas lista e faz todas as combinações possíveis das duas listas
combinacao :: [a] -> [b] -> [(a, b)]
combinacao xs ys = [(x,y)| x <- xs , y <- ys]

--d) que dado um número, retorne a lista com todos os divisores desse número.
divisor :: Int -> [Int]
divisor x = [ y | y <- [1..(x `div` 2)] , x `mod` y == 0 ]  

--5 Dado que uma fórmula é uma lista de lista, em que a lista mais interna é uma combinação de es
--e a lista mais externa é uma combinação de ous, dena a função avalia :: [[Bool]] -> Bool que
--transforma a lista de lista em um único valor de verdadeiro ou falso.
--Exemplo: (V e F e V) ou V ou (F e F) = [[True, False, True], [True], [False, False]]
avalia :: [[Bool]] -> Bool
avalia x = undefined

--6. Dado as questões 3 e 5 dena as funções:
--(a) for2lst :: Formula -> [[Bool]] que transforma uma formula em uma lista de lista de booleano.
{-for2lst :: Formula -> [[Bool]]
for2lst = undefined

--(b) lst2for :: [[Bool]] -> Formula que transforma uma lista de lista de booleano em uma formula.
lst2for :: [[Bool]] -> Formula
lst2for = undefined 
-}
--7 Dena a func~ao iteracao :: (a -> a) -> a -> [a] que cria uma lista innita de aplicac~ao de func~ao.
--iteracao f a = f a : f (f a) : f (f (f a)) : :::
iteracao :: (a -> a) -> a -> [a]
iteracao f x = x : iteracao f (f x)


--8. Utilizando somente func~ao de alta ordem (foldr, filter, (+3), etc), dena as seguinte func~oes:
--(a) replace :: Eq a ) a -> a -> [a] -> [a] que dado o primeiro e segundo par^ametro, troca onde tiver 
-- o valor do primeiro par^ametro pelo o valor do segundo par^ametro.
replace :: a -> a -> [a] -> [a]
replace = undefined

{--(b) int2bit0 :: Int -> [Bit]
int2bit0 :: Int -> [Bit]
int2bit0 0 = []
int2bit0 x = y : int2bit0 (x `div` 2)
    where y = x `mod` 2
--}

--(c) allEven0 :: [Int] => [Int]
allEven0 :: [Int] -> [Int]
allEven0 xs = filter(even) xs

--(d) avalia0 :: [[Bool]] -> Bool
avalia0 :: [[Bool]] -> Bool
avalia0 = undefined

--(e) soma :: [Int] -> Int, que dado uma lista de inteiros, retorna a soma de todos os elementos
soma :: [Int] -> Int
soma xs = last $ scanl1 (+) xs