-- David Seijas Pérez
-- Practica 2

--1
--a
cuadrados :: Integral a => a -> [a]
cuadrados 0 = [0]
cuadrados n = cuadrados(n-1) ++ [n*n]
    

--b
cuadrados2 :: Integral a => a -> [(a,a)]
cuadrados2 0 = [(0, 0)]
cuadrados2 n = (n, n*n) : cuadrados2(n-1)

--c
sumaCos :: (Floating a, Eq a) => a -> a  --Eq para poder hacer la comparacion con 0 en la primera guarda
sumaCos 0 = 0 
sumaCos n = n*abs(cos(n)) + sumaCos(n-1)

--d
sumaMul :: Integral a => a -> a
sumaMul n = sumaMul2 (n-1)
  where
  sumaMul2 :: Integral a => a -> a
  sumaMul2 m
    | m < 3                             = 0
    | (mod m 5 == 0) || (mod m 3 == 0)  = m + sumaMul2(m-1)
    | otherwise                         = sumaMul2(m-1)

--e
potenciasN :: Integral a => a -> Int
potenciasN n = length (filter f (filter g (map (^3) [0..n])))
               where f x = x < n
                     g y = mod y 100 == 43

--f
primosN :: Integral a => a -> a
primosN n = head (filter primo [n..]) 
            where primo x = not (any g (zipWith (mod) [x,x..] [2..(x-1)])) 
                  g z = z == 0


--2
--a
fcuadrados :: Integral a => a -> [a]
fcuadrados n = map (^2) [0..n] 

--b
fcuadrados2 :: Integral a => a -> [(a, a)]
fcuadrados2 n = zip [n,n-1..0] (map (^2) [n,n-1..0])

--c
fsumaCos :: Float -> Float
fsumaCos x = sum (zipWith (*) [1..x] (map (abs.cos) [1..x]))


--3
--a
iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = (map f [n..m]) == (map g [n..m])

--b

menorA :: Enum a => a -> a -> (a -> Bool) -> a
menorA n m p = head (filter p [n..m])

--c
mayor :: (Enum a, Num a) => a -> (a -> Bool) -> a  --Num para poder hacer el n-1
mayor n p = head (filter p [n,n-1..0])

--d
existe :: Enum a => a -> a -> (a -> Bool) -> Bool
existe n m p = any p [n..m]


--4
--a
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q = (filter p xs, filter q xs)

--b
--filters :: [a] -> [(a -> Bool)] -> [[a]]
filters xs [] = []
filters xs (p:ps) = (filter p xs) : filters xs ps


