-- Ejericio 1

-- I
max2 :: (Float, Float) -> Float
max2 (x,y) | x >= y = x
           | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

substract :: Float -> Float -> Float
substract = flip (-)

predecesor :: Float -> Float
predecesor = substract 1

evaluarEnCero :: (Float -> a) -> a
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

flipAll :: [a -> b-> c] -> [b -> a -> c]
flipAll = map flip

flipRaro :: (a -> b -> c) -> (b -> (a -> b -> c) -> c) -> b -> c -- PREGUNTAR: el tipo
flipRaro = flip flip

-- II
curryMax2 :: Float -> Float -> Float
curryMax2 = curry max2 

curryNormaVectorial :: Float -> Float -> Float
curryNormaVectorial = curry normaVectorial


-- Ejercicio 2

-- I
curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x, y)

-- II
uncurry2 :: (a -> b -> c) -> ((a, b) -> c)
uncurry2 f = \(x, y) -> f x y

-- III
-- Capaz haciendo una lista (1..n) y de esta manera tendriamos un curryN del tipo ((1..n) -> c)


-- Ejercicio 3

-- I
sumFold :: [Int] -> Int
sumFold = foldr (+) 0

elemFold :: Int -> [Int] -> Bool
elemFold e = foldr (\x r -> x == e || r) False

masMasFold :: [a] -> [a] -> [a]
masMasFold xs ys = foldr (:) ys xs

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = foldr (\x r -> if p x then x:r else r) []

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x r -> f x : r) []

-- II
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p2 = foldr1 (\x r -> if p2 x r then x else r)

-- III
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\ac x -> 
                        if null ac 
                            then [x] 
                            else ac ++ [x + last ac]) []

-- IV
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

substractFold :: Num a => [a] -> a
substractFold = foldr (flip (-)) 0 -- esto solo para terminar de entender

-- V
inverseSumaAlt :: Num a => [a] -> a
inverseSumaAlt = sumaAlt . reverse

inverseSumaAlt2 :: Num a => [a] -> a
inverseSumaAlt2 = foldl (flip (-)) 0


-- Ejercicio 4

-- I
insertarEnTodasLasPosiciones :: a -> [a] -> [[a]]
insertarEnTodasLasPosiciones e [] = [[e]]
insertarEnTodasLasPosiciones e (x:xs) = (e:x:xs) : map (x:) (insertarEnTodasLasPosiciones e xs)

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x r -> concatMap (insertarEnTodasLasPosiciones x) r) [[]]

-- II 
partes :: [a] -> [[a]]
partes = foldr (\x r -> r ++ map (x:) r) [[]]


--- III
prefijos :: [a] -> [[a]]
prefijos = foldl (\ac x -> ac ++ [last ac ++ [x]]) [[]]

--- IV
prefijosNoVacios :: [a] -> [[a]]
prefijosNoVacios = (drop 1) . prefijos

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = (sublistas xs) ++ prefijosNoVacios (x:xs)

-- recr (\x xs r -> r ++ prefijosNoVacios (x:xs))

-- Ejercicio 5

-- I) No es posible ya que no se hace recursion sobre la cola, sino la cola de la cola, eso no es usar la estructura

-- II
entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x r ys -> if null ys
                                then x : r []
                                else x : head ys : r (tail ys)) id


-- Ejercicio 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z []       = z
recr f z (x : xs) = f x xs (recr f z xs)

-- a.
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r -> if x == e then xs else x:r) []

-- b. No es adecuado porque utiliza la cola, y no la recursion sobre la cola

-- c.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r -> if e < x 
                                        then e:x:xs 
                                        else x:r) [e]


-- Ejercicio 7

-- I
genLista :: a -> (a -> a) -> Int -> [a]
genLista i f 0 = []
genLista i f c = i : genLista (f i) f (c-1)

-- II
desdeHasta :: (Int, Int) -> [Int]
desdeHasta (x, y) = genLista x (+1) (y-x+1)


-- Ejercicio 8

-- I
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\x r -> (uncurry f) x : r) []

-- II
armarPares :: [a] -> [b] -> [(a, b)]
armarPares _ [] = []
armarPares [] _ = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

armarParesFR :: [a] -> [b] -> [(a, b)]
armarParesFR = foldr (\x r ys -> if null ys
                                    then [] 
                                    else (x, head ys) : r (tail ys)) (const [])

-- III
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = (\xs -> (mapPares f) . armarPares xs)

-- mapDoble f xs ys = mapPares f (armarPares xs ys)


-- Ejercicio 9

-- I
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat [] _ = []
sumaMat (x:xs) (y:ys) = zipWith (+) x y : sumaMat xs ys

sumaMatFR :: [[Int]] -> [[Int]] -> [[Int]]
sumaMatFR = foldr (\x r ys -> zipWith (+) x (head ys) : r (tail ys)) (const [])

-- II
agregarACadaLista :: [a] -> [[a]] -> [[a]]
agregarACadaLista [] _ = []
agregarACadaLista xs [] = [[x] | x <- xs]
agregarACadaLista (x:xs) (y:ys) = (y++[x]) : agregarACadaLista xs ys

trasponer :: [[Int]] -> [[Int]]
trasponer = foldl (flip agregarACadaLista) []

trasponerFR :: [[Int]] -> [[Int]]
trasponerFR m = foldr (\x r -> mapDoble (++) (map (:[]) x) r) (replicate (length (head m)) []) m

trasponerFR2 :: [[Int]] -> [[Int]]
trasponerFR2 m = foldr (\x r -> zipWith (:) x r) (replicate (length (head m)) []) m


-- Ejercicio 10
generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs   = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

-- I
generateBase ::([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop i next = generate2 stop 
        (\l -> if null l then i else next (last l))

-- II
factoriales :: Int -> [Int]
factoriales n = generate2 ((n<) . length) 
        (\l -> if null l then 1 else length l * last l)

-- III
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase ((n<) . length) x f

-- IV
generateFrom2 :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom2 stop next xs = last (takeWhile (not . stop) (iterate (\l -> l ++ [next l]) xs))

generate2 :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate2 stop next = generateFrom2 stop next []


-- Ejercicio 11
data Nat = Zero | Succ Nat

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

instance Show Nat where
  show Zero = "Zero"
  show (Succ n) = "Succ (" ++ show n ++ ")"

doble :: Nat -> Nat
doble Zero     = Zero
doble (Succ n) = Succ (Succ (doble n))

-- I
foldNat :: (Int -> a -> a) -> a -> Int -> a
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

-- foldNat :: (Nat -> a -> a) -> a -> Nat -> a
-- foldNat f z Zero = z
-- foldNat f z (Succ n) = f (Succ n) (foldNat f z n)

-- II
potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia a n = a * potencia a (n-1)

potenciaFN :: Int -> Int -> Int
potenciaFN a = foldNat (\_ r -> a * r) 1


-- Ejercicio 12
data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)

-- Esquema estructural
foldPol :: Num a => (a -> a) -> a -> Polinomio a -> a
foldPol p z (Cte a) = z
foldPol p z (Suma a b) = p ((foldPol p z a) + (foldPol p z b))
foldPol p z (Prod a b) = p ((foldPol p z a) * (foldPol p z b))

evaluar :: Num a => a -> Polinomio a -> a
evaluar x = foldPol id 0


