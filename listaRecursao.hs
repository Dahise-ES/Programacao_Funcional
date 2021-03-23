{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import Prelude hiding ((^))
import qualified Prelude as P 
--1 

--fatorial 0 = 1
--fatorial n = n * fatorial (n - 1)

fatorial2 n 
    | n == 0 = 1
    | otherwise = n * fatorial2 (n - 1)


somar :: Int -> Int
somar x
    | x == 0 = 0 
    | otherwise = x + somar (x - 1)

(^) :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
m ^ 0 = 1
m ^ 1 = m
m ^ n = m * m ^ (n-1)

exponencial :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
exponencial m n 
    | n == 0 = 1
    | n == 1 = m 
    | otherwise = m * exponencial m (n -1)


-- 4
--euclides :: Int -> Int -> Int

euclides x y 
    | x == y = x
    | x > y = euclides (x - y) y
    | x < y = euclides x (y - x)


-- 5

--and :: [Bool] -> Bool
istrue x
    | x == True = True 
    | otherwise = False 

all_is_true xs = all istrue xs

allTrue :: [Bool] -> Bool 
allTrue xs
    | xs == [] = True 
    | head xs == True = allTrue (tail xs)
    | otherwise = False


--concat :: [[a]] -> [a]
   
concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- replicate :: Int -> a -> [a]
replicar 0 _ = []
replicar n v = v : replicar(n-1) v

-- (!!) :: [a] -> Int -> a
(@@) :: [a] -> Int -> a
xs @@ 0 = head xs
xs @@ n = tail xs @@ (n-1)

-- elem :: Eq a => a -> [a] -> Bool
elemento v (x:xs)
    | v == x = True 
    | otherwise = elemento v xs
elemento _ [] = False

-- merge :: Ord a => [a] -> [a] -> [a]

juntarOrd [] ys = ys
juntarOrd xs [] = xs
juntarOrd (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- 7
-- metades :: [a] -> ([a],[a])
metades xs = splitAt (length xs `div` 2) xs

--mergesort :: Ord a => [a] -> [a] 
mergesort [] = []
mergesort [x] = [x]
mergesort xs = juntarOrd (mergesort esquerda) (mergesort direita)
               where
                   lados    = metades xs
                   esquerda = fst lados
                   direita  = snd lados
                   --(esquerda, direita) = metades xs

--8

somaList :: [Int] -> Int 
somaList [] = 0
somaList (x:xs) = x + somaList xs

pedaco :: Int -> [a] -> [a]
pedaco 0 _  = []
pedaco _ [] = []
pedaco n (x:xs) = x : pedaco (n-1) xs

ultimo :: [a] -> a
ultimo [x] =x
ultimo (x:xs) = ultimo xs
