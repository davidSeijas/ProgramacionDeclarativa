--Practica 3
--DAVID SEIJAS PEREZ

--1
last' :: [a] -> a
last' = foldl (\x y -> y) undefined
--es igual que poner last' xs = foldl (\x y -> y) undefined xs
--como es l el primer parametro (aqui undefined sustituye a la x y luego, la y la van sustituyendo los elementos de izq a der.
--si fuese r undefined sustituiria a la y y a la x la sustituirían los elementos de x de derecha a izq

--last' (x:xs) = foldl (\x y -> y) x (x:xs)

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x : xs) []

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x y -> p x && y) True

minimum' :: Ord a => [a] -> a
minimum' (x:xs) = foldr (\x y -> min x y) x xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x : xs else []) []

append' :: [a] -> [a] -> [a]
append' xs ys = foldr (\x l -> x:l) ys xs


--2
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

--otra: mifoldr1 f xs = foldr f (last xs) (init xs)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f [x] = x
foldl1 f (x:y:xs) = foldl1 f ((f x y) : xs) 

--otra: mifoldl1 f (x:xs) = foldl f x xs


--3
--a
a3 = [x*y | x <- [1..], y <- [1,-1]]
--Se recorre todos los numeros desde el 1 para cada uno escribe ese multiplicado por 1 y luego por -1

a3' = [x | y <- [1..], x <- [y,-y]]
a3'' = concat [[k, (-k)] | k <- [1..]]
a3''' = concat $ zipWith (\x y -> [x,y]) [1..] [-1,-2..]

--b
b3 = [(x,y-x) | y <- [0..], x <- [0..y]]
--Primero recorro la suma que me tienen que dar los dos elementos de la tupla y para cada numero
--natural de esos pongo en el primer elemento un numero que va desde el 0 hasta la suma y en el 
--segundo lo que faltaria para sumar el numero natural por el que va la y

--b3' = [(x,y) | z <- [0..], x <- [0..z], y <- [0..z], x+y=z]


--4
--a
sufijos :: [a] -> [[a]]
sufijos xs = [drop x xs | x <- [0..(length xs)]]

sufijos2 xs = [ys | ys <- take (length xs + 1) $ iterate tail xs]
--muestro todas las listas que me salen de iterar tail a mi lista (es decir cada vez me quita un elemento), pero 
--de esa lista de iterate solo me cojo longitud(xs)+1 listas pues después dará excepción al intentar hacer tail []

sufijos3 [] = [[]]
sufijos3 xs = xs : (sufijos3 $ tail xs)


prefijos :: [a] -> [[a]]
prefijos xs = [take x xs | x <- [0..(length xs)]]

--b
sublistas :: [a] -> [[a]]
sublistas xs = [] : [take x ys | x <- [1..(length xs)], ys <- sufijos xs, x <= length ys]
--para la lista de sufijos de la nuestra, cojo sublistas desde el incio desde longitud 1 hasta la longitud de cada lista sufijo
--(sino es hasta la longitud de esta se repetirian elementos) y añadimos la lista vacia (si hacemos que se cojan sublitas
--desde logitud 0 nos aparecería la lista vacía muchas veces (tantas como listas de sufijos tengamos))

--sin ordenar
sublistas2 [] = [[]]
sublistas2 (x:xs) = sublistas2 xs ++ [x:ys | ys <- sublistas2 xs]  

--c
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x:ys | x <- xs, ys <- permutaciones ([y | y <- xs, y /= x])]
--hago todas las listas que fijan cada elemento de la lista y el resto (de los que son distintos de el) los permuta 

--d
sumandos :: (Eq a, Num a, Enum a) => a -> [[a]]
sumandos 0 = [[]]
sumandos n = [x:ys | x <- [1..n], ys <- sumandos(n-x)]