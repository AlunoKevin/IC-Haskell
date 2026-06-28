

module ArvoreBinaria.Arvore(ABB(..),searchABB, insertABB, removeABB, tamABB, hightABB, printABB) where

data ABB a = Vazio 
           | No a (ABB a) (ABB a) 
           deriving (Show, Eq, Ord)

searchABB :: (Ord a) => a -> ABB a -> Bool
searchABB _ Vazio = False
searchABB x (No a esq dir)
    | x == a = True
    | x < a = searchABB x esq
    | x > a = searchABB x dir
    | otherwise = False


insertABB :: (Ord a) => a -> ABB a -> ABB a
insertABB x Vazio = No x Vazio Vazio
insertABB x (No a esq dir)
    | x == a = No a esq dir
    | x < a = No a (insertABB x esq) dir
    | x > a = No a esq (insertABB x dir)
    | otherwise = No a esq dir


-- tive dificuldade em fazer essa funcao utilizei ajuda externa 
-- problema foi na remocao da raiz portanto a logica de remocao deveria ser differnte e manter as propriedades da abb
removeABB :: (Ord a) => a -> ABB a -> ABB a
removeABB _ Vazio = Vazio
removeABB x (No a esq dir)
    | x < a     = No a (removeABB x esq) dir
    | x > a     = No a esq (removeABB x dir)  
    | otherwise = case (esq, dir) of
        (Vazio, Vazio) -> Vazio
        (Vazio, _)     -> dir
        (_, Vazio)     -> esq
        (_, _)         -> let (minDir, newDir) = removeMin dir
                          in No minDir esq newDir


removeMin :: ABB a -> (a, ABB a)
removeMin (No a Vazio dir) = (a, dir)
removeMin (No a esq dir)   = let (minVal, newEsq) = removeMin esq
                             in (minVal, No a newEsq dir)
removeMin Vazio = error "removeMin called on an empty tree"

tamABB :: ABB a -> Integer
tamABB Vazio = 0
tamABB (No _ esq dir) = 1 + tamABB esq + tamABB dir -- mt ruim a recursao aqui 
-- caucula o numero de elementos da arvore binaria de busca


-- so necessario caucular o maximo entre ambos lados e apos adicionar a raiz remanecente
hightABB :: ABB a -> Integer
hightABB Vazio = -1
hightABB (No _ esq dir) = 1 + max (hightABB esq) (hightABB dir) -- mt ruim a recursao aqui 


printABB :: (Show a) => ABB a -> String
printABB Vazio = ""
printABB (No a esq dir) = printABB esq ++ show a ++ "" ++ printABB dir
