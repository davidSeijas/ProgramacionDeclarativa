--Practica 4 bis
--David Seijas

--1
data Direccion = Arriba | Abajo | Izquierda | Derecha
  deriving (Eq, Ord, Show)

type Coordenada = Int
type Punto = (Coordenada, Coordenada)

destino :: Punto -> [Direccion] -> Punto
destino = foldl move

move :: Punto -> Direccion -> Punto
move (x, y) Arriba     = (x, y+1)
move (x, y) Abajo      = (x, y-1)
move (x, y) Izquierda  = (x-1, y)
move (x, y) Derecha    = (x+1, y)

trayectoria :: Punto -> [Direccion] -> [Punto]
trayectoria p [] = [p] 
trayectoria p (x:xs) = let y = move p x in y:(trayectoria y xs)

inferior :: Int -> [Direccion] -> [Direccion] -> Bool
inferior n xs ys = and $ map (uncurry comparaTr) [(trayectoria' n (x,y) xs, trayectoria' n (x,y) ys) | x <- [0..n], y <- [0..n]] 

trayectoria' :: Int -> Punto -> [Direccion] -> [Punto]
trayectoria' _ p [] = [p]
trayectoria' n p (x:xs) = let y = move p x in if (0 <= fst y && fst y <= n) && (0 <= snd y && snd y <= n) then y:(trayectoria' n y xs) else p:(trayectoria' n p xs)

comparaTr :: [Punto] -> [Punto] -> Bool
comparaTr [] [] = True
comparaTr [] y = True
comparaTr x [] = True
comparaTr (x:xs) (y:ys) = if (fst x == fst y) then if (snd x >= snd y) then (comparaTr xs (y:ys)) && (comparaTr (x:xs) ys) else False
                                              else (comparaTr xs (y:ys)) && (comparaTr (x:xs) ys)

--llevo una lista de pares con las trayectorias (es la trayectoria de antes pero haciendo que si se salieran del plano el punto se queda en el mismo sitio)
--de cada lista de movimientos, cada es par es para un punto del plano. A esa lista le aplico comparaTr que compara que cada punto la segunda trayectoria
--esta siempre por debajo de todos los puntos de la primera (es decir, por debajo de los que comparte coordenada x). Esto lo hago con todas las trayectorias
--resultantes de habérselo aplicado a cada punto del plano. Si para alguno de los puntos, la segunda trayectoria supera a la primera se devuelve false.

--2
data Arbol a = Hoja a | Nodo a [Arbol a] 

--a
listaHojas :: Arbol a -> [a]
listaHojas (Hoja x) = [x]
listaHojas (Nodo x ts) = concat $ map listaHojas ts

listaNodos :: Arbol a -> [a]
listaNodos (Hoja x) = []
listaNodos (Nodo x ts) = x : (concat $ map listaNodos ts)

repMax :: Ord a => Arbol a -> Arbol a
repMax t = cambiar m t 
  where m = maximo (listaNodos t ++ listaHojas t)

maximo :: Ord a => [a] -> a
maximo [] = error "Lista vacia"
maximo [x] = x
maximo (x:xs) = if x >= (maximo xs) then x else (maximo xs)

cambiar :: Ord a => a -> Arbol a -> Arbol a
cambiar c (Hoja x) = Hoja c
cambiar c (Nodo x ts) = Nodo c (map (cambiar c) ts)

--b
--necesito que sea de la clase Eq para que sea de la clase Ord
instance Eq a => Eq (Arbol a) where
  Hoja x == Hoja y = x == y
  Hoja x == _ = False
  _ == Hoja y = False
  (Nodo x t) == (Nodo y t') = (x == y) && t == t'       --se puede hacer t<= t' pues queda definido el == para arboles y para listas es justo como queremos (elemento a elemento)

instance Ord a => Ord (Arbol a) where
  Hoja x <= Hoja y = x <= y
  Hoja x <= _ = True
  _ <= Hoja y = False
  (Nodo x t) <= (Nodo y t') = (x < y) || ((x == y) && t <= t')       --se puede hacer t<= t' pues queda definido el <= para arboles y para listas es justo como queremos (elemento a elemento)

--c
instance Show a => Show (Arbol a) where
  show (Hoja x) = " " ++ show x ++ " "
  show (Nodo x t) = " " ++ show  x ++ " " ++ show t 

