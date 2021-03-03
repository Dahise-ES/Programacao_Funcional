--1
--soma_dos_quadrados :: [a] -> a
somaDosQuadrados = sum [x ^ 2 | x <- [1 .. 100]]


--2
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x, y)| x <- [0 .. x], y <- [0 .. y]] 

--3
quadrado :: Int -> [(Int, Int)]
quadrado z = [(x, y) | (x, y) <- grid z z, x /= y]

--4

replicar :: (Num t, Enum t) => t -> a -> [a]
replicar n a = [a | _ <- [1 .. n]]

--5
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]

--6
fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], n `mod` x == 0]

perfeitos :: Int -> [Int]
perfeitos x = [ v | v <- [1 .. x], v == sum( init (fatores v))]

--7

--ainda não consegui fazer

--8
buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

posicoes :: (Eq a1, Num a2, Enum a2) => a1 -> [a1] -> [a2]
posicoes x xs = [i | i <- buscar x (zip xs [0 ..])]

--9
produtoEscalar xs ys = sum [ x * y | (x,y) <- zip xs ys]