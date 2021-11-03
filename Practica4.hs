--Practica 4
--DAVID SEIJAS

--1
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord, Show)

type Coordenada = Integer
type Punto = (Coordenada, Coordenada)

destino :: Punto -> [Direccion] -> Punto
destino = foldl move

move :: Punto -> Direccion -> Punto
move (x, y) Arriba     = (x, y+1)
move (x, y) Abajo      = (x, y-1)
move (x, y) Izquierda  = (x-1, y)
move (x, y) Derecha    = (x+1, y)


--2
data Nat = Cero | Suc Nat 
  deriving (Eq, Ord)

infixl 6 +.
(+.) :: Nat -> Nat -> Nat
Cero +. x = x
x +. Cero = x
(Suc x) +. y = Suc (x +. y)

infixl 7 *.
(*.) :: Nat -> Nat -> Nat
Cero *. x = Cero
x *. Cero = Cero
(Suc x) *. y = (x *. y) +. y

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

instance Show Nat where
 show x = show (natToInt x) 


--3 
type Re = Int
type Im = Int
data Complejo = Cp (Re, Im) deriving Eq

instance Num Complejo where
 Cp (a, b) + Cp (c, d) = Cp (a+b, c+d)
 Cp (a, b) - Cp (c, d) = Cp (a-b, c-d)
 Cp (a, b) * Cp (c, d) = Cp (a*c - b*d, a*d + c*b)

instance Show Complejo where
 show (Cp (a, b))
   | b == 1     = show a ++ "+" ++ "i"
   | b >= 0     = show a ++ "+" ++ show b ++ "i"
   | b == -1    = show a ++ "-" ++ "i"
   | otherwise  = show a ++ "-" ++ show (abs b) ++ "i"


--4
class Medible a where
 medida :: a -> Int

instance Medible Bool where
 medida True = 1
 medida False = -1

instance Medible Int where 
 medida x = x

instance Medible a => Medible [a] where
 medida []     = 0
 medida (x:xs) = medida x + medida xs

instance (Medible a, Medible b) => Medible (a,b) where
 medida (x, y) = medida x + medida y - 2
