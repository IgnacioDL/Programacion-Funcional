{-# LANGUAGE BangPatterns #-}

--Ignacio Díaz Lara

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

-- Parte (a)
apps :: Double  -> Double -> [Double]
apps a0 r = a0 : apps res r
          where res = 0.5 * (a0 + (r / a0))

-- Parte (b)
--función auxiliar para calcular el módulo
modulo :: (Ord a, Num a) => a -> a
modulo n |n == 0  = 0
         |n < 0   = n * (-1)
         |n > 0   = n

approxLimit :: Double -> [Double] -> Double
approxLimit _ []     = 0
approxLimit e (x1:xs) | modulo (x2 - x1) <= e  = x2
                      | otherwise              = approxLimit e xs
                      where x2 = head xs

-- Parte (c)
approxSqrt :: Double -> Double -> Double -> Double
approxSqrt r a0 e = approxLimit e (apps a0 r)



{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

data Tree a = Node a [Tree a]

-- Parte (a)
itTree :: (a -> [a]) -> a -> Tree a
itTree f root = Node root (map (itTree f) (f root))

-- Parte (b)
fnAux :: Integer -> [Integer]
fnAux a = (a+1) : (fnAux (a+1))

infTree :: Tree Integer
infTree = itTree (fnAux) 0


{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}
{-
f :: [Int] -> (Int, Int) -> (Int, Int)
f []     c = c
f (x:xs) c = f xs (step x c)

step :: Int -> (Int,Int) -> (Int, Int)
step n (c0, c1) | even n    = (c0, c1 + 1)
                | otherwise = (c0 + 1, c1)
-}

-- Parte (a): call-by-value
{-
  f [1,2,3,4] (0,1)
= 
  ->{def f.2}
  f [2,3,4] (step 1 (0,1))
  ->{def step (innermost redex)}
  f [2,3,4] (0 + 1, 1)
  -> {def +}
  f[2,3,4] (1,1)
  ->{def f.2}
  f [3,4] (step 2 (1,1))
  -> {def step}
  f[3,4] (1, 1 + 1)
  ->{def +}
  f[3,4] (1,2)
  ->{def f.2}
  f[4] (step 3 (1,2))
  -> {def step}
  f[4] (1 + 1, 2)
  ->{def +}
  f[4] (2,2)
  -> {def f.2}
  f[] (step 4 (2,2))
  ->{def step}
  f[] (2, 2 + 1)
  -> {def +}
  f[] (2,3)
  -> {def f.1}
  (2,3) 

-}

-- Parte (b): lazy evaluation
{-
  f [1,2,3,4] (0,1)
= 
  f [2,3,4] (step 1 (0,1))
  ->{def f.2}
  f [3,4] (step 2 (step 1 (0,1)))
  ->{def f.2}
  f [4] (step 3 (step 2 (step 1 (0,1))))
  ->{def f.2}
  f [] (step 4 (step 3 (step 2 (step 1 (0,1)))))
  ->{def f.1}
  (step 4 (step 3 (step 2 (step 1 (0,1)))))
  ->{def step}
  (step 4 (step 3 (step 2 (0 + 1,1)))))
  ->{def step}
  (step 4 (step 3 (0 + 1,1 + 1))))
  ->{def step}
  (step 4 ((0 + 1) + 1,1 + 1)))
  ->{def step}
  ((0 + 1) + 1),((1 + 1) + 1))
  ->{def +}
  ((1 + 1),((1 + 1) + 1))
  ->{def +}
  (2,(2 + 1))
  ->{def +}
  (2,3)

-}



{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}
-- Parte (a)
{-
Sí presenta un space leak, porque genera toda la suma dentro de cada
elemento de la tupla y genera una aplicación compuesta del step tan
grande como el tamaño de la lista.

-}

-- Parte (b)

f :: [Int] -> (Int, Int) -> (Int, Int)
f []      c = c
f (x:xs) !c = f xs (step x c)

step :: Int -> (Int,Int) -> (Int, Int)
step n (!c0, !c1) | even n    = (c0, c1 + 1)
                  | otherwise = (c0 + 1, c1)

-- Parte (c)
{-
Teniendo,

length = length2 0

length2 n [ ] = n
length2 n (x : xs) = if n==0 then length2 1 xs else length2 (n+1) xs


Viendo un poco de su reducción
length [1..10]
length2 0 [1..10]
length2 1 [2..10]
length2 (1+1) [3..10]
length2 2 [3..10]
length2 (2+1) [4..10]
length2 3 [4..10]
length2 (3+1) [5..10]
...

Hasta aquí queda claro que no se genera la suma de números que
normalmente forman un space leak debido a que la condición
n==0 obliga a resolver la suma para poder comparar. Si solamente
estuviera la llamada recursiva sin el n==0, entonces sí se 
generaría la suma de +1.

-}

-- Parte (d)
{-

De todas formas, se puede reescribir length2 para forzar la suma
de otra forma:

length = length2 0

length2 n [ ] = n
length2 n (x : xs) = n2 `seq` length2 n2 xs
                   where n2 = n+1
-}


{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
{-
DefiniciÃ³n original de partMerge
partMerge :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
partMerge p (xs,ys) = (filter p xs ++ filter (not . p) ys,
                       filter (not . p) xs ++ filter p ys)

EspecificaciÃ³n de  partcat
partcat p xs (us,vs) = (filter p xs ++ us, filter (not . p) xs ++ vs)
-}

-- Parte (a)
{-
El problema que tiene en términos de eficiencia es que recorre la lista
xs e ys dos veces.
-}

-- Parte (b)
{-
partMerge p (xs,ys)
-> {def partMerge}
(filter p xs ++ filter (not . p) ys, filter (not . p) xs ++ filter p ys)
->{def partcat}
partcat p xs (filter (not . p) ys, filter p ys)
->{def partcat a lo de adentro notando que deberían tener el argumento cambiado 
eso equivale a poner not . p en el parcat}
partcat p xs (partcat (not . p) ys [])
-}

-- Parte (c)
partcat :: (a -> Bool) -> [a] -> ([a],[a]) -> ([a],[a])
partcat p xs (us,vs) = ((foldl (\xs x -> if p x then reverse (x:xs) else xs) [] xs) ++ us, 
                        (foldl (\xs x -> if (not.p) x then reverse (x:xs) else xs) [] xs) ++ vs)


-- Parte (d)
{-
En términos de tiempo foldl' no aportaría nada, pues forzar la adhesión del elemento
provoca que se haga la misma cantidad de computación pero la diferencia es cuándo la hace.
Al forzar posibles adhesiones en el acumulador, hace que no se vaya apilando las adhesiones
a la lista y por lo tanto no se generan space leaks y se mejora la eficiencia en espacio.
-}



{-------------------------------------------}
{--------------  EJERCICIO 6  --------------}
{-------------------------------------------}
{-
DefiniciÃ³n original
h :: Int -> Int
h 0 = 2
h 1 = 3
h 2 = 4
h n = 1 + 3 * h (n-1) + 3 * h(n-3)
-}

-- Parte (a)
{-
El inconveniente que tiene la definición en tiempo de reducción
es que va a terminar calculando algunos valores repetidas veces.
Por ejemplo, para calcular h(8) se necesitan h(7) y h(5), pero
para calcular h(7) se necesita h(6) y h(4), y h(6) necesita, a su
vez, h(5) y h(3). Aquí se empieza a repetir e h(5) y más adelante
se repetirán más valores aún, que se necesitarán computar más 
de una vez.
-}

-- Parte (b)
-- calcula en una tripla (?) el resultado que buscamos y los dos siguientes
hAux :: Int -> (Int, Int, Int)
hAux 0 = (2,3,4)
hAux n = (fpn, fn, 1 + 3*fn + 3*fppn) 
       where (fppn, fpn, fn) = hAux(n-1)

-- función auxiliar para sacar el último elemento de la "tripla"
thirdTriple :: (Int, Int, Int) -> Int
thirdTriple (k, l, m) = m

{-calculo la tripla para n-2, ya que va a entregar los resultados para
(n-2, n-1, n) en ese orden, y sacando el último elemento no se calcula
más de la cuenta, el inconveniente es que necesito fijar h de 0 y 1, 
para que no intente calcular bajo cero-}
h :: Int -> Int
h 0 = 2
h 1 = 3
h n = thirdTriple (hAux (n-2))


main :: IO()
main = pure()