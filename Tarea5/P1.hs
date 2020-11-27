import Data.Array
import Control.Parallel.Strategies
import Control.Exception
import Data.List.Split

type Coeff = Integer
type Poly = Array Int Coeff

mkPoly :: [Coeff] -> Poly
mkPoly xs = listArray (0,d) xs where d = length xs - 1

degree :: Poly -> Int
degree = snd . bounds

coeff :: Poly -> Int -> Coeff
coeff p i | i <= degree p = p ! i
          | otherwise     = 0


-- parte (a)
coeffProd :: Poly -> Poly -> Int -> Coeff
coeffProd p q i = coeffProdAux p q i i 0

coeffProdAux :: Poly -> Poly -> Int -> Int -> Int -> Coeff
coeffProdAux p q i j c | j==c      = (coeff p 0) * (coeff q i)
                       | otherwise = (coeff p j) * (coeff q (i-j)) + coeffProdAux p q i (j-1) c

seqProd :: Poly -> Poly -> Poly
seqProd p1 p2 = mkPoly (map (coeffProd p1 p2) [0..d])  where
  d = degree p1 + degree p2


-- parte (b)
-- Elapsed time: 0.813s


-- parte (c)
parProd :: Poly -> Poly -> Poly
parProd p1 p2 = mkPoly (map (coeffProdPar p1 p2 0) [0..d] `using` parListChunk 15 rpar) where
    d  = degree p1 + degree p2

coeffProdPar :: Poly -> Poly -> Int -> Int -> Coeff
coeffProdPar p q j i | j==i      = (coeff p j) * (coeff q (i-j))
                     | otherwise = (coeff p j) * (coeff q (i-j)) + coeffProdPar p q (j+1) i


-- parte (d)
-- Elapsed time: 0.596s


-- parte (e)
-- Speedup: 0.813s/0.596s =1.36409396


-- parte (f)
par1Prod :: Poly -> Poly -> Poly
par1Prod p1 p2 = mkPoly (map (coeffProdPar1 p1 p2 0) [0..d] `using` parList rpar) where
    d  = degree p1 + degree p2     

coeffProdPar1 :: Poly -> Poly -> Int -> Int -> Coeff
coeffProdPar1 p q j i | j==i      = (coeff p j) * (coeff q (i-j))
                      | otherwise = (coeff p j) * (coeff q (i-j)) + coeffProdPar1 p q (j+1) i

-- parte (g)
-- Elapsed time: 0.592
-- Speedup: 0.813s/0.592s = 1.37331081

-- parte (h)
{-
El speed parece prácticamente nulo, comparado con paralelizar para montoncitos de 15,
porque la cantidad de núcleos utilizados está cercano (o es) el límite teórico de
"máximo paralelismo". Como tuvo un pequeño incremento, todavía podría ser que límite 
sea para 4 núcleos por ejemplo, en ese caso dos núcleos estaría bastante cercano al 
máximo speedup posible; o bien podría ser que dos núcleos sea la cantidad mínima de 
núcleos para el máximo speedup posible. En dicho caso no se puede aumentar el speedup 
independiente si se usan más núcleos. Cabe mencionar que ese límite está determinado
por la cantidad de secuencialidad en el programa.
-}

-- parte (i)
{-
Porque para que efectivamente se produzca la evaluación, y no quedé expresada por la
evaluación lazy de haskell, se necesita que la expresión (parProd pa pb) sean utilizada
posteriormente. 
Un uso como nonNullCoeff, al comparar los elementos en (parProd pa pb), 
obliga a que esos elementos sean calculados. En el caso de simplemente imprimir
en pantalla, sobre todo algo tan grande como (parProd pa pb) en este caso, añade
mucha computación secuencial y se pierde el objetivo del ejemplo, si bien, también 
obliga a hacer el cálculo.
-}


-- Determina el nÃºmero de coeficientes no nulos de un polinomio
nonNullCoeff :: Poly -> Int
nonNullCoeff = foldr (\c rec -> if (c == 0) then rec else rec + 1) 0

main :: IO()
main = do
  let pa = mkPoly [100..2000]
      pb = mkPoly [2000..5000]
  print (nonNullCoeff (seqProd pa pb))
  return ()