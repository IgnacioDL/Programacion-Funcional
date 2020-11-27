{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck(modifyMaxSize)

--Ignacio Díaz Lara

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

type Assoc k v = [(k,v)]
type Error     = String

-- Parte (a)
find :: (Eq k, Show k, Eq v) => k -> Assoc k v -> Either Error v
find key table | length valueList == 1                      = Right value
               | length valueList >= 1 && filteredList == 0 = Right value
               | length valueList >= 1                      = Left errorMultiple
               | length valueList <  1                      = Left errorNotFound
                where valueList        = [v' | (k',v')<-table, k'==key]
                      value            = head valueList
                      filteredList     = length (filter (/=value) valueList)
                      {-En esta lista se filtra el primer valor, si no
                        hay una key repetida, filteredList estará vacía-} 
                      errorMultiple    = "Multiple values for key " ++ show key
                      errorNotFound    = "Key " ++ show key ++ " not found"


-- Parte (b)
{-
1. Eq k se necesita para poder comparar las claves que se llaman key,
sin Eq k no se podría establecer que una key es igual a otra como
se hace para construir valueList.
2. Show k se requiere para poder mostrar la clave key que es el
significado que buscamos. Sin Show k no se puede usar show key 
para construir los strings de los errores.
3. Eq v es necesaria para poder comparar los todos los valores que 
de las keys que coinciden con la buscada, para poder saber si estos 
valores son repeticiones de la tupla o inconsistencias, o sea, para
la misma key diferentes valores. En el caso que son repeticiones de 
la tupla, se entrega el valor que corresponde, si hay inconsistencias
se entrega un error. Sin Eq v no se puede hacer el filtro en la lista 
de valores correspondientes a las keys exitosas.
-}



{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

type Variable  = Char
data Formula   = Const Bool
               | Var Variable
               | Not Formula
               | And Formula Formula
               | Imply Formula Formula
type Valuation = Assoc Char Bool


-- Parte (a)



-- Parte (b)
-- eval :: Formula -> Valuation -> Bool
-- Descomente el tipo y agregue su definiciÃ³n

-- fvar :: Formula -> [Char]
-- Descomente el tipo y agregue su definiciÃ³n


-- Parte (c)
-- isTaut :: Formula -> Maybe Valuation
-- Descomente el tipo y agregue su definiciÃ³n

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
-- here I can also do x: filter (/= x) (rmdups xs)
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) r ++ map (True :) r where
  r = bools (n-1)

-- Cuando haya implementado fvar, puede descomentar allVals
{-
allVals :: Formula -> [Valuation]
allVals f = map (zip vars) vals where
  vars = rmdups (fvar f)
  vals = bools (length vars)
-}



{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Peg = L | C | R deriving (Eq, Show)
type Disk = Int
type Conf = Peg -> [Disk]
type Move = (Peg,Peg)

instance Show Conf where
  show c = show (c L, c C, c R)

c :: Conf
c L = [1 .. 5]
c C = [ ]
c R = [ ]

push :: Disk -> Peg -> Conf -> Conf
push disk L conf = newConf where
  newConf L = disk:(conf L)
  newConf C = conf C
  newConf R = conf R
push disk C conf = newConf where
  newConf L = conf L
  newConf C = disk:(conf C)
  newConf R = conf R
push disk R conf = newConf where
  newConf L = conf L
  newConf C = conf C
  newConf R = disk:(conf R)

pop :: Peg -> Conf -> Conf
pop L conf = newConf where 
  newConf L = tail (conf L)
  newConf C = conf C
  newConf R = conf R
pop C conf = newConf where
  newConf L = conf L
  newConf C = tail (conf C)
  newConf R = conf R
pop R conf = newConf where
  newConf L = conf L
  newConf C = conf C
  newConf R = tail (conf R)

-- Parte (a)
step :: Move -> Conf -> Conf
step (peg1, peg2) conf | length (conf peg1) == 0 = error "Error: trying to move from empty peg"
                       | null (conf peg2)        = newConf
                       | disk > head (conf peg2) = error "Error: trying to move disk above an smaller disk"
                       | otherwise               = newConf where
  disk    = head (conf peg1)
  conf2   = pop peg1 conf
  newConf = push disk peg2 conf2


-- Parte (b)
optStrategy :: Int -> Move -> Conf -> [(Move,Conf)]
optStrategy n (peg1, peg2) conf = zip listMove listConf where
  listMove = optAuxMoves n (peg1, peg2)
  listConf = optAuxConfs listMove conf

optAuxMoves :: Int -> Move -> [Move]
optAuxMoves 0 _            = []
optAuxMoves n (peg1, peg2) = (optAuxMoves (n-1) (peg1, peg3)) ++ [(peg1, peg2)] ++ (optAuxMoves (n-1) (peg3, peg2)) where
  peg3 = thirdPeg (peg1, peg2)

optAuxConfs :: [Move] -> Conf -> [Conf]
optAuxConfs [] _        = []
optAuxConfs (m:ms) conf = newConf : (optAuxConfs ms newConf) where newConf = step m conf

--función auxiliar para saber cuál es el peg pivote
thirdPeg :: Move -> Peg
thirdPeg (L, R) = C
thirdPeg (R, L) = C
thirdPeg (C, R) = L
thirdPeg (R, C) = L
thirdPeg (L, C) = R
thirdPeg (C, L) = R

-- Una vez que haya implementado optStrategy
-- puede descomentar las siguientes dos funciones

makeInit :: Int -> Peg -> Conf
makeInit n p p' | p' == p   = [1..n]
                | otherwise = []

play :: Int -> Peg -> Peg -> IO()
play n s t = putStr $ show initConf ++ foldr f v (optStrategy n (s,t) initConf) where
  initConf  = makeInit n s
  v         = []
  f (m,c) r = "\n -> " ++ show m ++ " -> " ++ show c ++ r

-- Parte (c)
others :: Peg -> (Peg,Peg)
others L = (R,C)
others C = (L,R)
others R = (L,C)

instance {-# OVERLAPPING #-} Arbitrary Move where
  arbitrary = do
    s <- frequency [(1,return L), (1,return C), (1,return L)]
    t <- let (x1,x2) = others s in frequency [(1,return x1), (1,return x2)]
    return (s,t)

testoptStrategy :: Spec
testoptStrategy = describe "Optimal strategy for Hanoi Tower:" $ modifyMaxSize (const 10) $ do
  it "Configuraciones generadas son validas" $
                                  -- reemplace lo que sigue a ==> por su codigo
    property $ \n (s,t) -> 1 <= n ==> checkConf (optStrategy (n::Int) ((s,t)::Move) ((makeInit n s) :: Conf)) == True
  it "TamaÃ±o de la estrategia optima" $
                                  -- reemplace lo que sigue a ==> por su codigo
    property $ \n (s,t) -> 1 <= n ==> length (optStrategy (n::Int) ((s,t)::Move) ((makeInit n s) :: Conf)) == (2^n) - 1 

--función auxiliar que chequea que las configuraciones que entrega optStrategy estén ordenadas
checkConf :: [(Move,Conf)] -> Bool
checkConf []   = True
checkConf (x:xs) | listOrd (conf L) && listOrd (conf R) && listOrd (conf C) == True = checkConf xs
                 | otherwise                                                        = False 
                 where (move, conf) = x

--función auxiliar para saber si una lista está ordenada de menor a mayor
listOrd :: [Int] -> Bool
listOrd []  = True
listOrd [x] = True
listOrd (x:xs) | x > head xs = False
               | otherwise   = listOrd xs

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero     m = Zero
mult (Succ n) m = add m (mult n m)

foldNat :: (b -> b) -> b -> Nat -> b
foldNat f v Zero     = v
foldNat f v (Succ n) = f (foldNat f v n)


sumsqrAux :: Nat -> (Nat,Nat)
sumsqrAux = foldNat f v 
          where v       = (Zero, Zero)
                f (n,r) = (Succ n, add r ((Succ n) `mult` (Succ n)))

sumsqr :: Nat -> Nat
sumsqr = snd . sumsqrAux


{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
data BinTree a = Leaf a | InNode (BinTree a) a (BinTree a)

foldBT :: (b -> a -> b -> b) -> (a -> b) -> (BinTree a -> b)
foldBT f g (Leaf v)         = g v
foldBT f g (InNode t1 v t2) = f (foldBT f g t1) v (foldBT f g t2)

-- Parte (a)
{-
p.d.q. = h . foldBT f g = foldBT f' g'

caso base:
h . foldBT f b (Leaf v) = foldBT f' g' (Leaf v)

{Por el lado izquierdo}
h . foldBT f b (Leaf v)
{<- foldBT.1}
h . (g v) 
{definición composición}
h (g v)
{<- enunciado.1 => h (g v) = g' v}
g' v


{Por el lado derecho}
foldBT f' g' (Leaf v)
{<- foldBT.1}
g' v

Se llega a lo mismo por los dos lados, por lo tanto
se prueba el caso base inductiva y se tiene la 
hipótesis inductiva:

h . foldBT f g bt = foldBT f' g' bt

caso inductivo: 
h . foldBT f b (InNode t1 v t2) = foldBT f' g' (InNode t1 v t2)

{Por el lado izquierdo}
h . foldBT f b (InNode t1 v t2)
{<- foldBT.2}
h . (f (foldBT f g t1) v (foldBT f g t2))
{Definición composición}
h (f (foldBT f g t1) v (foldBT f g t2))
{<-Enunciado.2 => h (f x1 v x2) = f' (h x1) v (h x2)}
f' h (foldBT f g t1) v (h foldbT f g t2)
{Definición Composición}
f' (h . foldBT f g t1) v (h . foldbT f g t2)
{Hipóstesis inductiva x2}
f' (foldBT f' g' t1) v (foldBT f' g' t2)


{Por el lado derecho}
foldBT f' g' (InNode t1 v t2)
{<- foldBT.2}
f' (foldBT f' g' t1) v (foldBT f' g' t2)

Se llega a lo mismo por los dos lados, por lo tanto queda
demostrada la inducción estructural.
-}


-- Parte (b)
flattenBT :: BinTree a -> [a]
flattenBT = foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[])

sizeBT :: BinTree a -> Int
sizeBT = foldBT (\r1 v r2 -> r1 + 1 + r2) (const 1)

{-
p.d.q. = length . flattenBT = sizeBT

caso base: 
length . flattenBT (Leaf v) = sizeBT (Leaf v)

{Por el lado izquierdo}
length . flattenBT (Leaf v)
{<- flattenBt.1}
length . foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (Leaf v)
{<-Parte a) considerando h = lenght, f =(\r1 v r2 -> r1 ++ [v] ++ r2), g = (:[])}
foldBT f' g' (Leaf v)
{<- foldBT.1}
g' v
{<- Parte a).1 => h (g v) = g' v}
lenght((:[]) v)
{Definición de :}
lenght([v])
{Definición de length}
1


{Por el lado derecho}
sizeBT (Leaf v)
{<- sizeBT.1}
foldBT (\r1 v r2 -> r1 + 1 + r2) (const 1) (Leaf v)
{<- foldBT.1}
(const 1) (Leaf v)
{Definición de const}
1

Se llega a lo mismo por los dos lados, por lo tanto
se prueba el caso base inductiva y se tiene la 
hipótesis inductiva:

length . flattenBT bt = sizeBT bt

caso inductivo:

length . flattenBT (InNode t1 v t2) = sizeBT (InNode t1 v t2)

{Por el lado izquierdo}
length . flattenBT (InNode t1 v t2)
{<- flattenBt.1}
length . foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (InNode t1 v t2)
{<- foldBT.1 y resolviendo}
length . (flatten t1 ++ [v] ++ flatten t2) 
{<- Parte b) Hint => length (xs ++ ys) = length xs + length ys}
lenght (flatten t1) + length [v] + length (flatten t2)
{Hipóstesis inductiva x2 y definición de length}
sizeBT t1 + 1 + sizeBT t2


{Por el lado derecho}
sizeBT (InNode t1 v t2)
{<- sizeBT.1}
foldBT (\r1 v r2 -> r1 + 1 + r2) (const 1) (InNode t1 v t2)
{<- sizeBT.1 y resolviendo}
sizeBT t1 + sizeBT v + sizeBT t2
{<- sizeBT.1}
sizeBT t1 + 1 + sizeBT t2

Se llega a lo mismo por los dos lados, por lo tanto queda
demostrada la inducción estructural.
-}


-- Parte (c)
mirrorBT :: BinTree a -> BinTree a
mirrorBT = foldBT (\r1 v r2 -> InNode r2 v r1) Leaf

idBT :: BinTree a -> BinTree a
idBT = foldBT InNode Leaf

{-
p.d.q. = mirrorBT . mirrorBT = idBT

caso base: 
mirrorBT . mirrorBT (Leaf v) = idBT (Leaf v)

{Por el lado izquierdo}
mirrorBT . mirrorBT (Leaf v)
{<- mirrorBt.1}
mirrorBT . foldBT (\r1 v r2 -> InNode r2 v r1) Leaf (Leaf v)
{<- foldBT.1}
mirrorBT . Leaf (Leaf v)
{Definición de Leaf}
mirrorBT . (Leaf v)
{Definición Composición}
mirrorBT (Leaf v)
{<- mirrorBt.1}
foldBT (\r1 v r2 -> InNode r2 v r1) Leaf (Leaf v)
{<- foldBT.1}
Leaf (Leaf v)
{Definición de Leaf}
Leaf v


{Por el lado derecho}
idBT (Leaf v)
{<- idBT.1}
foldBT InNode Leaf (Leaf v)
{<- foldBT.1}
Leaf (Leaf v)
{Definición de Leaf}
Leaf v

Se llega a lo mismo por los dos lados, por lo tanto
se prueba el caso base inductiva y se tiene la 
hipótesis inductiva:

mirrorBT . mirrorBT bt = idBT bt

caso inductivo:

mirrorBT . mirrorBT (InNode t1 v t2) = idBT (InNode t1 v t2)

{Por el lado izquierdo}
mirrorBT . mirrorBT (InNode t1 v t2)
{<- mirrorBt.1}
mirrorBT . foldBT (\r1 v r2 -> InNode r2 v r1) Leaf (InNode t1 v t2)
{<-foldBT.1 y resolviendo}
mirrorBT . InNode t2 v t1
{Definición Composición}
mirrorBT (InNode t2 v t1)
{<- mirrorBt.1}
foldBT (\r1 v r2 -> InNode r2 v r1) Leaf (InNode t2 v t1)
{<-foldBT.1 y resolviendo}
InNode t1 v t2


{Por el lado derecho}
idBT  (InNode t1 v t2)
{<- idBT.1}
foldBT InNode Leaf (InNode t1 v t2)
{<- foldBT.2}
InNode (foldBT InNode Leaf t1) v (foldBT InNode Leaf t2)
{<-idBT.1 x2}
InNode (idBT t1) v (idBT t2)
{<-Hipótesis inductiva => mirrorBT . mirrorBT bt = idBT bt}
InNode (mirrorBT . mirrorBT t1) v (mirrorBT . mirrorBT t2)
{<-mirrorBT.1}
InNode (mirror . foldBT (\r1 v r2 -> InNode r2 v r1) Leaf t1) v (mirror . foldBT (\r1 v r2 -> InNode r2 v r1) Leaf t2)
{<-foldBt.2, además definiendo t1 = InNode s1 v1 s2, y, t2 = InNode p1 v2 p2}
InNode (mirror . InNode s2 v1 s1) v (mirror . InNode p2 v1 p1)
{Definición composición}
InNode (mirror (InNode s2 v1 s1)) v (mirror (InNode p2 v1 p1))
{<-mirrorBT.1}
InNode (foldBT (\r1 v r2 -> InNode r2 v r1) Leaf (InNode s2 v1 s1)) v (foldBT (\r1 v r2 -> InNode r2 v r1) Leaf (InNode p2 v1 p1))
{<- foldBT.2}
InNode (InNode s1 v1 s2) v (InNode p1 v1 p2)
{recordando t1 = InNode s1 v1 s2, y, t2 = InNode p1 v2 p2}
InNode t1 v t2

Se llega a lo mismo por los dos lados, por lo tanto queda
demostrada la inducción estructural.
-}


-- Parte (d)
mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f (Leaf v)         = Leaf (f v)
mapBT f (InNode t1 v t2) = InNode (mapBT f t1) (f v) (mapBT f t2)

{-
p.d.q. = map f (flattenBT t) = flattenBT (mapBT f t)
haciendo inducción sobre la estructura t

caso base:

map f (flattenBT (Leaf v)) = flattenBT (mapBT f (Leaf v))

{Por el lado izquierdo}
map f (flattenBT (Leaf v))
{<- flattenBT.1}
map f (foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (Leaf v))
{<- foldBT.1}
map f ((:[]) v)
{Definición :}
map f [v]
{Definición de map}
[f v]

{Por el lado derecho}
flattenBT (mapBT f (Leaf v))
{<- mapBT.1}
flattenBT(Leaf (f v))
{<- flattenBT.1}
foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) Leaf (f v)
{<- foldBT.1}
(:[]) Leaf (f v)
{Definición de :}
[Leaf (f v)]
{Definición de Leaf}
[f v]

Se llega a lo mismo por los dos lados, por lo tanto
se prueba el caso base inductiva y se tiene la 
hipótesis inductiva:

map f (flattenBT bt) = flattenBT (mapBT f bt)

caso inductivo:

map f (flattenBT (InNode t1 v t2)) = flattenBT (mapBT f (InNode t1 v t2)) 

{Por el lado izquierdo}
map f (flattenBT (InNode t1 v t2))
{<- flattenBT.1}
map f (foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (InNode t1 v t2))
{<- foldBT.2 y resolviendo}
map f (flattenBT t1 ++ [v] ++ flattenBT t2)
{<- Hint Parte d) =>  map f (xs ++ ys) = map f xs ++ map f ys}
map f (flattenBT t1) ++ map f [v] ++ map f (flattenBT t2)
{Definición map}
map f (flattenBT t1) ++ [f v] ++ map f (flattenBT t2)


{Por el lado derecho}
flattenBT (mapBT f (InNode t1 v t2))
{<- mapBT.2}
flattenBT (InNode (mapBT f t1) (f v) (mapBT f t2))
{<- flattenBT.1}
foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (InNode (mapBT f t1) (f v) (mapBT f t2))
{<-foldBT.2 y resolviendo}
flattenBT  (mapBT f t1) ++ [(f v)] ++ flattenBT mapBT f t2
{Hipóstesis inductiva x2}
map f (flattenBT t1) ++ [f v] ++ map f (flattenBT t2)

Se llega a lo mismo por los dos lados, por lo tanto queda
demostrada la inducción estructural.
-}



main :: IO()
main = hspec testoptStrategy