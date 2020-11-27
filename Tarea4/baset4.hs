import Control.Monad.State
import Numeric.Probability.Distribution hiding (map,coin,filter)

--Ignacio Díaz Lara

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}
type Matrix = [[Int]]
type Size = Int

sourceMatrix :: Matrix
sourceMatrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

targetMatrix :: Matrix
targetMatrix = [[14,16,15,13],[6,8,7,5],[2,4,3,1],[10,12,11,9]]

-- parte (a)
swapRow :: Int -> Int -> Matrix -> Matrix
swapRow row1 row2 matrix = swap row1 row2 matrix

swapColumn :: Int -> Int -> Matrix -> Matrix
swapColumn _    _    []     = []
swapColumn col1 col2 (x:xs) = (swap col1 col2 x) : swapColumn col1 col2 xs

swap :: Int -> Int -> [a] -> [a]
swap x y | x == y = id
         | otherwise = swap' (min x y) (max x y)

swap' :: Int -> Int -> [a] -> [a]
swap' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
  where
    (beginning, (x : r)) = splitAt first lst
    (middle, (y : end)) = splitAt (second - first - 1) r


-- parte (b)
swapM :: Size -> Matrix -> [Matrix]
swapM size matrix = colswap ++ rowswap where 
    colswap = do (n,m) <- listTuplasAux (size-1)
                 return (swapColumn n m matrix)
    rowswap = do (n,m) <- listTuplasAux (size-1)
                 return (swapRow n m matrix)
                     
{-- genera una lista de tuplas de todas las combinaciones que
se necesitan para intercambiar filas y columnas pero sin las que provocarían
una matriz repetida--}
listTuplasAux :: Int -> [(Int,Int)]
listTuplasAux 1    = [(1,0)]
listTuplasAux size = (listTuplasAux2 size size) ++ listTuplasAux (size-1)

listTuplasAux2 :: Int -> Int -> [(Int,Int)]
listTuplasAux2 size 1 = [(size, 0)]
listTuplasAux2 size x = (size, x-1) : listTuplasAux2 size (x-1)
 
-- parte (c)
swapMUntil :: Size -> (Matrix -> Bool) -> (Int, [Matrix]) -> (Int, [Matrix])
swapMUntil size p (c, matList) | length target >= 1 = (c, target)
                               | otherwise          = swapMUntil size p (c+1, concat matrixlist)
                               where target     = filter p matList
                                     matrixlist = matList >>= \m -> return (swapM size m)


answer :: Int
answer = fst (swapMUntil 4 (== targetMatrix) (0,[sourceMatrix]))


{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}
type Disk = Int 
data Peg = L | C | R deriving (Eq, Show)
type Conf = ([Disk], [Disk], [Disk])
type Move = (Peg,Peg)


-- parte (a)
push :: Disk -> Peg -> State Conf Conf
push disk L = state (\(x, y, z) -> ((disk:x, y, z), (disk:x, y, z)))
push disk C = state (\(x, y, z) -> ((x, disk:y, z), (x, disk:y, z)))
push disk R = state (\(x, y, z) -> ((x, y, disk:z), (x, y, disk:z)))

pop :: Peg -> State Conf Disk
pop L = state (\(x:xs, y, z) -> (x, (xs, y, z)))
pop C = state (\(x, y:ys, z) -> (y, (x, ys, z)))
pop R = state (\(x, y, z:zs) -> (z, (x, y, zs)))

-- parte (b)
step :: Move -> State Conf Conf
step (peg1, peg2) = do disk <- pop peg1
                       push disk peg2

-- parte (c)
getPegAux :: Move -> Peg
getPegAux move |elem L move && elem R move = C
               |elem L move && elem C move = R
               |elem C move && elem R move = L

optStrategy :: Int -> Move -> State Conf [(Move,Conf)]
optStrategy 1 (peg1, peg2) = do conf <- step (peg1, peg2)
                                return [((peg1, peg2), conf)]
optStrategy n (peg1, peg2) = do r <- optStrategy (n-1) (peg1, (getPegAux (peg1, peg2)))
                                s <- optStrategy 1 (peg1, peg2)
                                t <- optStrategy (n-1) ((getPegAux (peg1, peg2)), peg2)
                                return (r ++ s ++ t) 

-- parte (d)
{-
La diferencia principal que veo es la legibilidad y elegancia, la definición
es más compacta. A pesar de que en términos de funcionamiento no creo que sea
una mejora sustancial, sí presenta beneficios que no tiene la otra gracias
a la monada de estado. Yo me quedaría con esta forma por los beneficios de 
la monada de estado (por ejemplo si quisiera saber cuantos pasos toma)
y porque es más legible el código.
-}


-- parte (e)
{- 

--No me dio el tiempo para terminar de que tipara

play :: Int -> Peg -> Peg -> IO()
play n s t = putStr $ show initConf ++ foldr f v (optStrategy n (s,t)) where
  initConf  = makeInit n s
  v         = []
  f [((m,c), r)] = "\n -> " ++ show m ++ " -> " ++ show c ++ show r
-}
makeInit :: Int -> Peg -> Conf
makeInit n p | p==L = ([1..n], [], [])
             | p==C = ([], [1..n], [])
             | p==R = ([], [], [1..n])


{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}
type Probability = Rational
type Dist a = T Probability a


-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
pointDist r = do x <- uniform [-r..r]
                 y <- uniform [-r..r]
                 return (x,y)

-- Parte (a.II)
resultE3a :: Int -> Probability
resultE3a r = 4*probC where
    pts   = pointDist r
    probC = (condicion r) ?? pts 

condicion :: Int -> (Int, Int) -> Bool
condicion r (x,y)| (x^2 + y^2) <= r^2 = True
                 | otherwise          = False 



-- Parte (b)
{-
Si le resulta conveniente, puede empezar siguiendo
el hint dado:-}

data Uni     = Chile | Cato deriving (Eq,Ord,Show)
type Urn     = (Int, Int)
-- 1er componente: #jugadores Chile, 2do componente: #jugadores Cato

--crea una lista de Uni repetidos tantas veces como jugadores existan por cada lado
auxiliar1 :: Urn -> [Uni]
auxiliar1 (0,0) = []
auxiliar1 (0, uc) = Cato : auxiliar1 (0, uc-1)
auxiliar1 (uch, uc) = Chile : auxiliar1 (uch-1, uc)

pickPlayer :: Urn -> Dist (Uni,Urn)
pickPlayer (uch, uc) = uniform (auxiliar1 (uch, uc)) >>= f where 
    f Cato  = return (Cato, (uch, uc-1))
    f Chile = return (Chile, (uch-1, uc))

bernUni :: (Int, Int) -> Dist Uni
bernUni (uch, uc) = choose (m/(m+n)) Chile Cato where
    n = toRational uc
    m = toRational uch 

{- Explicación: con el pickPlayer original se generaba toda la lista, como por 
ejemplo [Cato, Cato, Chile, Chile], y a eso se le aplica uniforme.
Pero eso hace que listaComb (la función que genera todas las combinaciones 
del sorteo) se caiga para más de 9 jugadores. Hasta 9 aguanta bien, pero justo
se pedían 10 y ahí muere. 
pickPlayer2 usa bernoulli para alivianar la computación, pero su uso aislado 
tiene problemas de formato para cuando hay 0 de parte de alguna universidad. 
Para que compute basta con que se use la primera versión de pickPlayer en 
esos casos.
-} 
pickPlayer2 :: Urn -> Dist (Uni,Urn)
pickPlayer2 (0, uc) = pickPlayer (0,uc)
pickPlayer2 (uch,0) = pickPlayer (uch,0)
pickPlayer2 (uch, uc) = bernUni (uch,uc) >>= f where 
    f Cato  = return (Cato, (uch, uc-1))
    f Chile = return (Chile, (uch-1, uc))

listaComb :: Urn -> Dist [Uni]
listaComb (0,0) = pure []
listaComb (n,m) = do (a,b) <- pickPlayer2 (n,m)
                     (:) <$> return a <*> listaComb b

condicionB :: [Uni] -> Bool
condicionB []  = True
condicionB [x] = True
condicionB lista | l1 == Cato && l2 == Cato = False
                 | otherwise                = condicionB (drop 2 lista) 
                 where l1 = head lista
                       l2 = head (drop 1 lista )

resultE3b :: Probability
resultE3b = condicionB ?? lista where 
    lista = listaComb (8,2)

main :: IO()
main = return ()