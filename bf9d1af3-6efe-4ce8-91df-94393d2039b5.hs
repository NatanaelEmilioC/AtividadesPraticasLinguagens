import Data.Bits

--1)
repetir :: a -> [a]
repetir a = as where as = a:as

--2
--a)
type Bit = Int

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit x = x `mod` 2 : int2bit (x `div` 2)

--b)
bit2int :: [Bit] -> Int
bit2int xs = sum [ x * y | (x, y) <- zip tamanho xs] where tamanho = iterate (*2) 1

--3 Crie um tipo de dados que represente uma fórmula, sendo uma fórmula sendo:
--Verdadeiro
-- Falso
-- E, necessário duas fórmulas
-- Ou, necessário duas fórmulas
-- Não necessário uma fórmula
--Dado que foi criado o tipo de dado Fórmula, dena a função eval :: Formula -> Bool que recebevuma fórmula e a avalia.
{-
data Term t (deriving Eq) where
    Con     ::          a                               -> Term a
    E       ::      Term Bool           -> Term Bool    -> Term Bool 
    Ou      ::      Term Bool           -> Term Bool    -> Term Bool 
    Smaller ::      Term Int            -> Term Int     -> Term Bool
    Plus    ::          Term Int        -> Term Int     -> Term Int

data Formula ts where
Body   :: Term Bool -> Formula ()
Forall :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)

eval :: Term t -> t
--eval (Con i) =i
eval (E p q)=eval p && eval q
eval (Ou p q)=eval p || eval q
--eval(Smaller n m)=eval n < eval m
--eval (Plus n m)    = eval n + eval m
-}
--4 Utilizando somente compressão de lista - List Comprehension - dena as seguintes funções:
--a)que recebe uma lista e retorna um lista somente com números inteiros.
allEven :: [Int] -> [Int]
allEven = undefined

--b) recebe uma lista de listas e retorna com o tamanho desse lista e a lista correspondente.
lengths :: [[a]] -> [(Int, [a])]
lengths = undefined

--c) que recebe duas lista e faz todas as combinações possíveis das duas listas
combinacao :: [a] -> [b] -> [(a, b)]
combinacao = undefined

--d) que dado um número, retorne a lista com todos os divisores desse número.
divisor :: Int -> [Int]
divisor = undefined

--5 Dado que uma fórmula é uma lista de lista, em que a lista mais interna é uma combinação de es
--e a lista mais externa é uma combinação de ous, dena a função avalia :: [[Bool]] -> Bool que
--transforma a lista de lista em um único valor de verdadeiro ou falso.
--Exemplo: (V e F e V) ou V ou (F e F) = [[True, False, True], [True], [False, False]]


--6. Dado as questões 3 e 5 dena as funções:
--(a) for2lst :: Formula -> [[Bool]] que transforma uma formula em uma lista de lista de booleano.
for2lst :: Formula -> [[Bool]]
for2lst = undefined

--(b) lst2for :: [[Bool]] -> Formula que transforma uma lista de lista de booleano em uma formula.
lst2for :: [[Bool]] -> Formula
lst2for = undefined 

--7 Dena a func~ao iteracao :: (a -> a) -> a -> [a] que cria uma lista innita de aplicac~ao de func~ao.
--iteracao f a = f a : f (f a) : f (f (f a)) : :::

--8. Utilizando somente func~ao de alta ordem (foldr, filter, (+3), etc), dena as seguinte func~oes:
--(a) replace :: Eq a ) a -> a -> [a] -> [a] que dado o primeiro e segundo par^ametro, troca onde tiver 
-- o valor do primeiro par^ametro pelo o valor do segundo par^ametro.
replace :: Eq a ) a -> a -> [a] -> [a]
replace = undefined

--(b) int2bit0 :: Int -> [Bit]
int2bit0 :: Int -> [Bit]
int2bit0 = undefined

--(c) allEven0 :: [Int] => [Int]
allEven0 :: [Int] => [Int]
allEven0 = undefined

--(d) avalia0 :: [[Bool]] -> Bool
avalia0 :: [[Bool]] -> Bool
avalia0 = undefined

--(e) soma :: [Int] -> Int, que dado uma lista de inteiros, retorna a soma de todos os elementos
soma :: [Int] -> Int
soma = undefined