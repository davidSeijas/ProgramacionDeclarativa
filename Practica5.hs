--Práctica 5
--David Seijas Pérez

--1
adivina :: Int -> IO ()
adivina n = do 
  putStrLn "Introduce un número"
  aux <- getLine
  x <- return (read aux::Int)
  if x < n then do 
    putStrLn "Tu número es menor que el buscado"
    adivina n
  else if x > n then do
    putStrLn "Tu número es mayor que el buscado"
    adivina n
  else do
    putStrLn "Número correcto"
    return ()


--2
formatea :: String -> String -> Int -> IO () 
formatea fileIn fileOut n = do
   input <- readFile fileIn
   let lineas = lines input
       lineasF = format lineas n
       output = unlines lineasF 
   putStrLn output
   writeFile fileOut output

format :: [String] -> Int -> [String]
format lineas n = let palabras = map words lineas
                      numBytes = map (length.concat) palabras
                      palabras' = zip palabras numBytes
                      palabrasAjustadas = ajustar palabras' n
                  in 
                      map concat palabrasAjustadas

ajustar :: [([String], Int)] -> Int -> [[String]]
ajustar [] n = []
ajustar (p:ps) n = if (snd p >= n) || (length (fst p) == 1) then (putSpaces (fst p) 1):(ajustar ps n) --las que se pasan se quedan igual
                                                            else (putSpaces (fst p) spaces):(ajustar ps n)
                                                                 where spaces = div (n - (snd p)) (length (fst p) - 1)  --hay 1 hueco menos que el número de palabras

putSpaces :: [String] -> Int -> [String]
putSpaces x 0 = x
putSpaces xs n = putSpaces (map (++ " ") xs) (n-1)


--3
type Vector = [Float]
type Matriz = [Vector]

--a
transp :: Matriz -> Matriz
transp [] = []
transp [fila] = map (:[]) fila                      -- tenemos un vector fila y pasamos a tener una columna (varios vectores de 1 elemento)
transp (fila:fs) = zipWith (:) fila (transp fs)     --cada elemento de una fila lo meto por orden en las respectivas nuevas filas que se han formado al trasponer la ultima fila de todas a una columna
--transp (fila:fs) = zipWith (++) (transp [fila]) (transp fs)

sumaMat :: Matriz -> Matriz -> Matriz
sumaMat m1 m2 = zipWith (zipWith (+)) m1 m2  

prodEscV :: Vector -> Vector -> Float               --producto escalar de vectores
prodEscV v1 v2 = if length v1 == length v2 then sum $ zipWith (*) v1 v2
                                           else error "Dimensión de matrices no compatibles"

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = [[prodEscV fila columna | columna <- (transp m2)] | fila <- m1] 

--b
dibujaMatriz :: Matriz -> IO ()
dibujaMatriz [] = return ()
dibujaMatriz (fila:fs) = do putStrLn(concat (map ((++ "     ").show) fila))
                            dibujaMatriz fs


-----comprobando que es matriz

data Matrix = M Int Int [[Float]] deriving Show

isMatrix :: Matrix -> Bool
isMatrix (M f c xs) = if f == length xs && (foldl (\x y -> x && c == length y) True xs) then True else False

--utilizaré las funciones de antes, si no estuvieran implementadas sería hacerlo como funciones auxiliares y el resutlado sería el mismo
--a
transponer :: Matrix -> Maybe Matrix
transponer (M 0 0 []) = Just (M 0 0 [])
transponer (M f c fs) =if (isMatrix (M f c fs)) then Just (M c f (transp fs)) else Nothing  

sumaMatrix :: Matrix -> Matrix -> Maybe Matrix
sumaMatrix (M f1 c1 m1) (M f2 c2 m2) = if (isMatrix (M f1 c1 m1)) && (isMatrix (M f2 c2 m2)) && f1==f2 && c1==c2 then Just (M (f1+f2) (c1+c2) (sumaMat m1 m2)) else Nothing

prodMatrix :: Matrix -> Matrix -> Maybe Matrix
prodMatrix (M f1 c1 m1) (M f2 c2 m2) = if (isMatrix (M f1 c1 m1)) && (isMatrix (M f2 c2 m2)) && f2==c1 then Just (M f1 c2 (prodMat m1 m2)) else Nothing 

--b
dibujaMatrix :: Matrix -> IO ()
dibujaMatrix (M f c []) = return ()
dibujaMatrix (M f c (x:xs)) = do putStrLn(concat (map ((++ "     ").show) x))
                                 dibujaMatrix (M f c xs)



