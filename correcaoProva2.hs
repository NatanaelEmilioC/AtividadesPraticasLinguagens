{--
Sistemas de Informação
Correção da Prova 2 de linguagens de programação

Aluno: Natanael Emilio da Costa
Matricula: 16.1.8298
--}

--1
--a) allOdd :: [Int] → [Int], que cria um lista contendo somente os valores impares da lista de entrada.
allOdd :: [Int] -> [Int]
allOdd lista = filter (\x -> x `mod` 2 == 1) lista

--b)  strip :: Eq a ⇒ [a] → [a] → [a] que elimina do segundo argumento, todos os elementos do primeiro argumento
strip :: Eq a => [a] -> [a] -> [a]
strip listaA listaB = filter(\x -> foldr (\ y z -> y /= x && z ) True listaA) listaB

--c) concatL :: [[a]] → [a] que concatena todas as listas uma lista
--concatL :: [[a]] -> [a]
cancatL listas = foldr(++) [] listas 

--d) tamanho :: [a] → Int que retorna o tamanho da lista
tamanho :: [a] -> Int
tamanho lista = foldr(+) 0 $ map(\x -> 1) lista

--2
--a) conta :: Eq a ⇒ a → [a] → Int que retorna a quantidade de vezes que o primeiro argumento acontece no segundo argumento
conta :: Eq a => a -> [a] -> Int
conta elemento lista = foldr(+) 0 [ 1 | y <- lista, y == elemento]

--b) ordenado :: Ord a ⇒ [a] → Bool que dado uma lista, retorna verdadeiro se a lista esta ordenada
ordenado :: Ord a => [a] -> Bool
ordenado (cabeca:cauda) = length cauda == foldr(+) 0 [ 1 | x <- cauda, ordenado cauda, cabeca <= (cauda !! 0) ]

--c)  tamanho :: [a] → Int que retorna o tamanho da lista
tamanho2 :: [a] -> Int
tamanho2 lista = foldr(+) 0 [ y | y <- lista, let y = 1]

--3
-- Seja uma árvore binária de busca representada da seguinte maneira:

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show
--a) Adicione um elemento a árvore
adicionaElemento :: (Ord a) => Tree a -> a -> Tree a
adicionaElemento Leaf raiz =  Node raiz Leaf Leaf
adicionaElemento (Node raiz esquerda direita) novo = if raiz <= novo then Node raiz esquerda (adicionaElemento direita novo) else Node raiz (adicionaElemento esquerda novo) direita

--b)  Compute o tamanho da árvore
tamanhoArvore :: Tree a -> Int
tamanhoArvore Leaf = 0
tamanhoArvore (Node _ esquerda direita) = 1 + tamanhoArvore esquerda + tamanhoArvore direita

--c) Aplica uma função em todos os elementos de uma árvore
mapT :: (a -> b) -> Tree a -> Tree b
mapT _ Leaf = Leaf
mapT funcao (Node raiz esquerda direita) = (Node (funcao raiz) (mapT funcao esquerda) (mapT funcao direita))