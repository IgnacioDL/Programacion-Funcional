import Test.Hspec
import Test.QuickCheck



{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

effPow :: (Num a, Ord a) => a -> Integer -> a
effPow b n | b == 0        = 0
           | n == 0        = 1
           | n < 0         = error "exponente negativo"
           | mod n 2 == 0  = (effPow b m) * (effPow b m) 
           | otherwise     = (effPow b m) * (effPow b m) * b where m = (div n 2)

pay :: Integer -> (Integer, Integer)
pay x | x < 8                      = error ("no se pueden pagar "++ show x ++" pesos")
      | mod x 3 == 0               = ((div x 3),0)
      | mod (mod x 5) 3 == 0       = (t,c) 
      | mod (mod x 3) 5 == 0       = (t,c) 
      | otherwise                  = (1+fst(pay (x-3)), snd (pay (x-3)))
      where t = div (mod x 5) 3 
            c = div (x-(t*3)) 5

{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

numberOfHits :: (a -> Bool) -> [a] -> Int
numberOfHits _ [] = 0
numberOfHits p (x:xs) | p x       = 1 + numberOfHits p xs
                      | otherwise = 0 + numberOfHits p xs

splitAtFirstHit :: (a -> Bool) -> [a] -> ([a] , [a])
splitAtFirstHit _ [] = error "no hit in the list"
splitAtFirstHit p (x:xs) | p x       = ([],x:xs)
                         | otherwise = (x : fst(splitAtFirstHit p xs), snd(splitAtFirstHit p xs))

positionsAllHits :: (a -> Bool) -> [a] -> [Int]
positionsAllHits _ [] = []
positionsAllHits p x | p (head(reverse x)) = b ++ [a]
                     | otherwise           = b
                     where a = (length x) - 1
                           b = positionsAllHits p (init x)

odds :: [Char] -> [Char]
odds []    = []
odds  (x:xs) = evens (xs)
evens :: String -> String
evens [] = []
evens (x:xs) = x: odds (xs)

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

hasSomeHit :: (a -> Bool) -> [a] -> Bool
hasSomeHit _ [] = False
hasSomeHit p (x:xs) | p x       = True
                    | otherwise = hasSomeHit p xs

isMember :: Eq a => a -> [a] -> Bool
isMember _ [] = False
isMember c (x:xs) | hasSomeHit (== c) [x]  = True
                  | otherwise              = isMember c xs

repeteadElem ::  Eq a => [a] -> Bool
repeteadElem [] = False
repeteadElem (x:xs) | hasSomeHit (== x) xs  = True
                    | otherwise             = repeteadElem xs

applyUntil :: (a->a) -> (a->Bool) -> a -> a 
applyUntil f p a | p a       = a
                 | p (f a)   = f a
                 | otherwise = applyUntil f p (f a)    

leastPow2 :: Integer -> Integer
leastPow2 n | n<0       = error "argumento negativo"
            | otherwise = applyUntil (*2) (>= n) 2

--funAux la usaré en balancedSufix para fijar la condición de término de applyUntil
funAux :: [Bool] -> Bool
funAux a | numberOfHits (==True) a == numberOfHits (==False) a   = True
         | otherwise                                             = False

balancedSufix :: [Bool] -> [Bool]
balancedSufix [] = []
balancedSufix x | numberOfHits (==True) x == numberOfHits (==False) x     = x
                | otherwise                                               = a
                where a = applyUntil balancedSufix funAux (tail x)


{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

--isMemberPF c x = hasSomeHit (== c) x
--isMemberPF c x = (hasSomeHit (== c)) x
--isMemberPF c = (hasSomeHit (== c))
isMemberPF :: Eq a => a -> [a] -> Bool -- con derivación
isMemberPF  c = hasSomeHit (== c)

{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}

testEffPow :: Spec
testEffPow = describe "effPow function:" $ do
      it "first property" $
            property $ \a b n -> n>=0 && a>=0 && b>=0 ==> effPow((a::Integer) * (b::Integer)) (n::Integer) == (effPow a n) * (effPow b n)
      it "second property" $
            property $ \m n a -> n>=0 && m>=0 ==> effPow (a::Integer) ((m::Integer) + (n::Integer)) == (effPow a m) * (effPow a n)

testPay :: Spec
testPay = describe "pay function:" $ do
      it "first property" $
            property $ \n -> n>=8  ==> ((3*fst(pay n)) + (5*snd(pay n))) == (n::Integer)

testNumberOfHits:: Spec
testNumberOfHits = describe "numberOfHits function:" $ do
      it "first property" $
            property $ \xs ys -> numberOfHits even (xs++ys) == ((numberOfHits even (xs::[Int])) + (numberOfHits even (ys::[Int])))
      it "second property" $
            property $ \xs -> numberOfHits (const True) (xs::[Int]) == length (xs)
      it "third property" $
            property $ \xs -> numberOfHits (const False) (xs::[Int]) == 0 


main :: IO()
main = hspec $ do
  testEffPow
  testPay
  testNumberOfHits
--  testExtraNumberOfHits


--Ignacio Díaz Lara