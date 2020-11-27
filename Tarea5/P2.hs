import Control.Parallel.Strategies
import Data.List.Split


type Number = Double

-- parte (a)
integral :: (Number -> Number) -> Number -> Number -> Int -> Number
integral f a b nInt = (h/2) * (integralAux f a b nInt nInt) where
    n = fromIntegral nInt
    h = (b - a)/n

integralAux :: (Number -> Number) -> Number -> Number -> Int -> Int -> Number
integralAux f a b n    0    = f(a) + f(a + h) where h = (b - a)/(fromIntegral n)
integralAux f a b nInt iInt = f(a + i*h) + f(a + (i + 1)*h) + integralAux f a b nInt (iInt-1) where
    i = fromIntegral iInt 
    n = fromIntegral nInt
    h = (b - a)/n

-- parte (b)
pintegral :: (Number -> Number) -> Number -> Number -> Int -> Number
pintegral f a b nInt = (h/2) * (pintegralAux f a b [0..nInt]) where
    n = fromIntegral nInt
    h = (b - a)/n

pintegralAux :: (Number -> Number) -> Number -> Number -> [Int] -> Number
pintegralAux f a b list = sum (map (integralAux2 f a b n) list `using` parListChunk 50 rpar) where
    n = length list

integralAux2 :: (Number -> Number) -> Number -> Number -> Int -> Int -> Number
integralAux2 f a b nInt iInt = f(a + i*h) + f(a + (i + 1)*h) where
    i = fromIntegral iInt 
    n = fromIntegral nInt
    h = (b - a)/n

-- parte (c)
-- Elapsed time sequential version: 0.052s
-- Elapsed time parallel version: 0.039s
-- Speedup: 0.052s/0.039s = 1.33333s


main :: IO()
main = do
  let f = \x -> 2*x^2 + 3*x^10 - x^6 + 10*x^30 - 8*x^25
      a = 0
      b = 100
      n = 20000
  print (pintegral f a b n)
  return ()