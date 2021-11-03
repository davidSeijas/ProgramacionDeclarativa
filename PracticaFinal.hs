--PRÁCTICA FINAL 
--DAVID SEIJAS PÉREZ

data Rel a = R [(a,a)] deriving (Read, Show)

r1 :: Integral a => Rel a
r1 = R [(1,1), (1,2), (1,4), (2,1), (2,2), (2,4), (3,3), (4,1), (4,2), (4,4)]  --rel de equivalencia

r2 :: Integral a => Rel a
r2 = R [(x,y) | x <- [1..10], y <- [1..10], (mod x 2) == (mod y 2)] --rel de equivalencia

r3 :: Rel Char
r3 = R [('a','b'), ('a','d'), ('b','d'), ('c','a'), ('d', 'c'), ('d','d')]

r4 :: Rel Char
r4 = R [('c','a'), ('a','d'), ('d', 'c'), ('b','d'), ('a','b'), ('d','d')]  --igual que r3

r5 :: Rel Char
r5 = R [('a','b'), ('d','a'), ('c','a'), ('d','c'), ('d','d')]  --distinta de r3

r6 :: Integral a => Rel [a]
r6 = R [([1, 2], []), ([2, -2], [3, 3, 1]), ([1, 2], [1, 3]), ([1, 3], [0]), ([4], [4]), ([1, 2], [0])] --transitiva

r7 :: Integral a => Rel a
r7 = R [(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 1), (3, 1), (4, 1), (5, 1)] --simétrica

r8 :: Integral a => Rel a
r8 = R ([(y, x) | x <- [1], y <- [1..5]] ++ [(x, y) | x <- [1], y <- [2..5]]) --igual que r7

r9 :: Integral a => Rel a
r9 = R [(1, 1)] --de equivalencia


--Voy a utilizar listas (sin repeticiones) como tipo que devuelven aquellas funciones que devuelvan un conjunto

mismoCj :: Eq a => [a] -> [a] -> Bool
mismoCj xs ys = all (`elem` ys) xs && all (`elem` xs) ys

--utilizaré esta función cuando quiera ver que dos listas representan el mismo conjunto


eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos xs = foldr (\x y -> if (notElem x y) then x:y else y) [] xs

--utilizaré esta función cuando necesite eliminar repetidos de una lista para que sea un conjunto
--eliminarRepetidos [] = []
--eliminarRepetidos (x:xs) = x:(eliminarRepetidos (filter (/= x) xs))




--EJERCICIO 1

esRelacion :: Eq a => Rel a -> Bool
esRelacion (R []) = True
esRelacion (R (x:xs)) = if elem x xs then False else esRelacion (R xs)





--EJERCICIO 2

instance Eq a => Eq (Rel a) where 
  R [] == R []    = True
  R xs == R ys    = mismoCj xs ys && esRelacion (R xs) && esRelacion (R ys) 





--EJERCICIO 3

dominio :: Eq a => Rel a -> [a]
dominio (R xs)
  |esRelacion (R xs)  = foldr (\x y -> if (notElem (fst x) y) then (fst x):y else y) [] xs
  |otherwise          = error "El argumento no es una relación"

--lo que hago es ir añadiendo los elementos de la izquierda de la relación en una nueva lista (mientras no se repitan)



soporte :: Eq a => Rel a -> [a]
soporte (R xs) 
  |esRelacion (R xs)  = foldr (\x y -> if (notElem (snd x) y) then (snd x):y else y) (dominio (R xs)) xs
  |otherwise          = error "El argumento no es una relación"                 
 where ys = unzip xs

--lo que hago es ir añadiendo los elementos de la derechade la relación en una nueva lista (mientras no se repitan)
--y a esos elementos les añado los del dominio que no estén todavía


relEquivalencia :: Eq a => Rel a -> Bool
relEquivalencia r 
  |esRelacion r  = esReflexiva r && esSimetrica r && esTransitiva r
  |otherwise     = error "El argumento no es una relación"


esReflexiva :: Eq a => Rel a -> Bool
esReflexiva (R xs)
  |esRelacion (R xs) = and [elem (x, x) xs | x <- soporte (R xs)]
  |otherwise         = error "El argumento no es una relación"

--veo que para cada elemento del conjunto sobre el que está definida la relacion, el elemento (x,x) está en la lista de la relacion

{-
Otras definiciones:
esReflexiva (R xs) = all (\x -> elem (x, x) xs) (soporte (R xs))

esReflexiva (R xs) = and $ map (`elem` xs)[(ys !! i, ys !! i) | i <- [0..length ys - 1]] 
                       where ys = soporte (R xs)
veo que para elemento del dominio, el par formado por ese elemento y él mismo exista en el conjunto de la relación
-}


esSimetrica :: Eq a => Rel a -> Bool
esSimetrica (R xs)
  |esRelacion (R xs)  = and [elem (y, x) xs | (x, y) <- xs]
  |otherwise          = error "El argumento no es una relación"

--veo que para TODA pareja de la relacion, la que cambia los elementos de orden tambien esta

{-
Otras definiciones:: 
esSimetrica (R xs) = all (\(x, y) -> elem (y, x) xs) xs

esSimetrica (R xs) = R xs == R (map (\x -> (snd x, fst x)) xs)
veo que el conjunto de las relaciones es igual a él mismo cambiando el orden de los pares de todas las 
relaciones (si es simétrica para cada relación (a,b) existirá (b,a) y las del tipo (a,a) se mantendrán igual)
-}


esTransitiva :: Eq a => Rel a -> Bool
esTransitiva (R xs)
  |esRelacion (R xs)  = and [elem (fst x, snd y) xs | x <- xs, y <- xs, snd x == fst y]
  |otherwise          = error "El argumento no es una relación" 

--emparejo cada relación de xs con otra tq (a,b) (b,c) y veo que (a,c) pertenecen a xs
--esTransitiva (R xs) = and [elem (x1, y2) xs | (x1, y1) <- xs, (x2, y2) <- xs, y1 == x2]

--comparo en todas si es relación para poder utilizarlas por separados para ver si alguna relación es reflexiva simétrica o transitiva


--Extra: antisimétrica, es decir, que si (x,y) e (y,x) están relacionados entonces x==y

esAntisimetrica :: Eq a => Rel a -> Bool
esAntisimetrica (R xs)
  |esRelacion (R xs)  = and [notElem (y, x) xs | (x, y) <- xs, x /= y]
  |otherwise          = error "El argumento no es una relación"




--voy a expresar el conjunto cociente como una lista donde los elementos son listas con los elementos de una misma clase de equivalencia

conjCociente :: Eq a => Rel a -> [[a]] 
conjCociente (R xs) 
  |relEquivalencia (R xs)   = eliminarRepetidos [[y | y <- dominio (R xs), elem (x, y) xs] | x <- dominio (R xs)]
  |otherwise                = error "La relacion no es de equivalencia"

--para cada elemento del dominio (= soporte por ser r de equivalencia) añado a una lista 
--sus elementos (hago su clase de equivalemcia) y elimino aquellas que se repitan


--Extra: funcion que devuelve la clase de quivalencia de un elemento de una rel de equivalencia

claseEquiv :: Eq a => Rel a -> a -> [a]
claseEquiv (R xs) x 
   |relEquivalencia (R xs) && (elem x (dominio (R xs)))  = [y | (x, y) <- xs]
   |otherwise                                            = error "La relación no es de equivalencia o el elemento no pertenece a ella"




generaDiv ::  Int -> Int -> Rel Int
generaDiv n m = R [(x, y) | y <- [n..m], x <- [n..y], mod y x == 0] 

{-
x hasta y pues los divisores de y son más pequeños que y 
(ademas el orden de x e y da igual pues la relacion es la misma independientemente del orden de sus elementos)
aqui no compruebo que vayan a salir repetidos pues no van a salir nunca

generaDiv n m = R [(x, y) | y <- [1..m], x <- [n..y], mod y x == 0] 
no sabia si la coma entre la x e y significa otra condicion o que las desigualdades eran para ambos
-}



generaGE :: Ord a => [a] -> Rel a
generaGE xs = R $ eliminarRepetidos [(x, y) | y <- xs, x <- xs, x >= y]




--Para el cierre haré un comentario. Un relación reflexiva tiene como cierre reflexivo a sí misma
--y esto ocurre también para las simétricas y transitivas y, por eso, una rel de equiv. es su propio cierreRTS

cierreRST :: Eq a => Rel a -> Rel a
cierreRST r
   |relEquivalencia r  = r
   |esRelacion r       = cierreT $ cierreS $ cierreR r
   |otherwise          = error "El argumento no es una relación"


cierreR :: Eq a => Rel a -> Rel a
cierreR (R xs)
   |esReflexiva (R xs)  = (R xs)
   |otherwise           = R (xs ++ [(x, x) | x <- soporte (R xs), notElem (x, x) xs])

--lo que hago es añadir a la relación los elementos (x, x) (siendo x del conjunto 
--sobre el que está definida la relación) que no esuvieran ya 


cierreS ::  Eq a => Rel a -> Rel a
cierreS (R xs)
   |esSimetrica (R xs)  = (R xs)
   |otherwise           = R (xs ++ [(y, x) | (x, y) <- xs, notElem (y, x) xs])

--lo que hago es añadir a la relación los elementos (y, x) si el elemento (x, y) 
--lo tenía definido en la relación y siempre que no esuvieran ya en mi relación


cierreT :: Eq a => Rel a -> Rel a
cierreT (R xs)
   |esTransitiva (R xs)  = (R xs)
   |otherwise            = R (xs ++ [(x1, y2) | (x1, y1) <- xs, (x2, y2) <- xs, x2 == y1, notElem (x1, y2) xs])

--lo que hago es añadir a la relación los elementos (a, c) siempre que los elemento (a, b) y (b, c) 
--estaban definidos en la relación y siempre que los (a, c) no estuvieran ya en mi relación


--con la composicion el cierre transitivo se puede hacer componiendo la relacion consigo misma hasta que no añade nada nuevo,
--siempre que la entrada sea algo simétrico y reflexivo (es decir, si hemos aplicado antes cierreR y cierreS)

cierreT' :: Eq a => Rel a -> Rel a
cierreT' r
   |r' == r         = r
   |otherwise       = cierreT' r'
   where r' = composicion r r




composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R xs) (R ys)
  |not(esRelacion (R xs)  && esRelacion (R ys))   = error "Los argumentos no son relaciones"
  |mismoCj (soporte (R xs)) (soporte (R ys))      = R (eliminarRepetidos [(fst x, snd y) | x <- xs, y <- ys, snd x == fst y])
  |otherwise                                      = error "Relaciones definidas sobre distintos conjuntos"

--la composicion será coger cada elemento de la primera relación con cada uno de los de la segunda y si sus elementos de la derecha y 
--de la izquierda, respectivamente, coincide entonces los compongo (izq del 1º, der del 2º) y al final elimino todos los repetidos





--EJERCICIO 4

--Voy a pedir que se introduzca el número de elementos de la relación y, hasta que metamos todos, se pida que se introducan los elementos de la forma adecuada.
--Luego para cada elemento que meto lo que hago es ir concatenándolos a una cadena que empeza "R [" y al terminar de concatenar todos añado "]".
--De esta forma tengo la relación, pero como string y lo que hago es un read indicándole que lo que devuelvo es del tipo Rel Int y ya he conseguido devolver una relación de enteros

introRel:: IO (Rel Int)
introRel = do 
  putStrLn "Introduce nº de elementos de la relación"
  aux <- getLine
  n <- return(read aux::Int)
  str <- introRel' "R [" n 
  return (read str::(Rel Int))                                 --si la relación y sus elementos no son de la forma adecuada, es decir, pares de elementos pares, se devolverá un parse error

introRel' :: String -> Int -> IO String
introRel' str n = do
  if n==0 then do
     return (str) 
  else do
     putStrLn "Introduce elemento (x,y) de la relación"
     aux <- getLine                                            --leo los elementos como String sin preocuparme de su tipo para añadirlos directamente a la cadena. Si el formato de estos es erróneo, me dará error la funcion principal 
     if n == 1 then do
        introRel' (str ++ aux ++ "]") (n-1)                    --dependiento de si es el último elemento o no lo añado a la cena con una ',' de separación de elementos o con ']' de fin de la relacion
     else do
        introRel' (str ++ aux ++ ",") (n-1)



muestraRel :: IO ()
muestraRel = do
   (R xs) <- introRel
   s <- return(soporte (R xs))
   putStrLn("    " ++ concat [show x ++ "  " | x <- s])        --cada elemento del soporte lo paso y le añado espacios despues (para la separacion entre elementos) y concateno el espacio inicial (es decir, paso de una lista de Int a una lista de String)
   putStrLn ("   " ++ concat ["---" | x <- s])                 --para cada elemento del soporte voy a escribir tres "-" formando la linea de "----------"
   escribeFilas xs s 0
   putStrLn ("   " ++ concat ["---" | x <- s])
   return ()


--esta funcion va a invocar a la función que escribe la fila de cada elemento del soporte con las x donde hay elemento de la relación
--cuando escriba todas va a retornar a la funcion principal

escribeFilas :: [(Int, Int)] -> [Int] -> Int -> IO () 
escribeFilas xs s cont = do
   if (cont == length s) then do 
      return ()
   else do 
      escribeFila xs s (s !! cont)
      escribeFilas xs s (cont+1)


--esta funcion me va a escribir la linea de un elemento del soporte. Primero muestro el elemento tal que "x  |" y al final añado "|". 
--En medio lo que hago es recorrer todos los elementos del soporte y ver si hay relacion con el elemento que paso como argumento.
--Dependiendo de si hay relación o no escribo " x " o "   " (dejando los espacios que en la primera fila separan los elementos)

escribeFila :: [(Int, Int)] -> [Int] -> Int -> IO ()
escribeFila xs s x = putStrLn (show x ++ " |" ++ (concat (map (\y -> if (elem (x,y) xs) then (" x ") else "   ") s)) ++ "|")



--Hago otra función igual, pero para comprobar con relaciones que ya estaban definidas
muestraRel' :: Rel Int -> IO ()
muestraRel' (R xs) = do
   s <- return(soporte (R xs))
   putStrLn("    " ++ concat [show x ++ "  " | x <- s])        
   putStrLn ("   " ++ concat ["---" | x <- s])                 
   escribeFilas xs s 0
   putStrLn ("   " ++ concat ["---" | x <- s])
   return ()

