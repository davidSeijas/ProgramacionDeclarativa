-- DAVID SEIJAS PÉREZ 
-- PRÁCTICA 1

-- Ejercicio 1
-- a
añosSec :: Float
añosSec = (10^6)/(60*60*24*365) 

--b
resultados :: (Int, Int, Int, Int, Int)
resultados = let segundos = mod (10^6) 60
                 min = div (10^6) 60
                 minutos = mod min 60
                 h = div min 60
                 horas = mod h 24
                 d = div h 24
                 dias = mod d 365
                 años = div d 365 
             in (años, dias, horas, minutos, segundos)

--c
calculaAños :: Float -> Float
calculaAños x = x/(60*60*24*365) 

calculaAños2 :: Int -> (Int, Int, Int, Int, Int)
calculaAños2 y = let segundos = mod y 60
                     min = div y 60
                     minutos = mod min 60
                     h = div min 60
                     horas = mod h 24
                     d = div h 24
                     dias = mod d 365
                     años = div d 365 
                 in (años, dias, horas, minutos, segundos)


--2
media :: Fractional a => [a] -> a
media (x:xs) = sum xs / fromIntegral (length xs)
media [] = error "Lista vacia"


--3
--a
num_digitos :: Int -> Int
num_digitos x = if (abs x >= 10) then (1 + num_digitos(div x 10)) else 1

--b
reduccion :: Int -> Int
reduccion x = if (abs x >= 10) then (reduccion (suma_digitos x)) else x
 where suma_digitos :: Int -> Int
       suma_digitos x 
         | abs x >= 10  = (mod x 10) + suma_digitos(div x 10)
         | otherwise    = x

--c
comb :: Int -> Int -> Int
comb n 0 = 1
comb n m 
  | n < 0 || m < 0      = error "Argumento negativo"
  | m > n               = error "Argumentos invalidos"
  | n == m              = 1
  | otherwise           = comb (n-1) m + comb (n-1) (m-1)


comb2 :: Int -> Int -> Int
comb2 n m = if (n > m) then div (factorial n) (factorial m * factorial (n-m)) else error "Argumentos invalidos"
 where factorial :: Int -> Int
       factorial x
         | x == 0     = 1
         | x > 0      = x*factorial (x-1)
         | otherwise  = error "Argumento negativo"


--4 
--estricta en el primer argumento y no en el segundo
conjuncion1 :: Bool -> Bool -> Bool 
conjuncion1 False _ = False
conjuncion1 _ False = False
conjuncion1 _ _ = True

--estricta en el segundo argumento y no en el primero
conjuncion2 :: Bool -> Bool -> Bool 
conjuncion2 _ False = False
conjuncion2 False _ = False
conjuncion2 _ _ = True

--estricta en los dos argumentos
conjuncion3 :: Bool -> Bool -> Bool 
conjuncion3 True y = y
conjuncion3 x True = x
conjuncion _ _ = False
