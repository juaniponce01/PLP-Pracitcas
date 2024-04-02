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
foldPol :: (a -> b) -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> a -> Polinomio a -> b
foldPol f cte suma prod x pol = case pol of
    (X) -> f x
    (Cte c) -> cte c
    (Suma a b) -> suma (recPol a) (recPol b)
        where recPol = foldPol f cte suma prod x
    (Prod a b) -> prod (recPol a) (recPol b)
        where recPol = foldPol f cte suma prod x

evaluar :: Num a => a -> Polinomio a -> a
evaluar = foldPol id id (+) (*)


-- Ejercicio 13
data AB a = Nil | Bin (AB a) a (AB a)

instance (Show a) => Show (AB a) where
  show Nil = "Nil"
  show (Bin i r d) = "(" ++ (show i) ++ " " ++ (show r) ++ " " ++ (show d) ++ ")"

-- I
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil 
foldAB cNil cBin (Bin i r d) = 
    cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB cNil cBin Nil = cNil
recAB cNil cBin (Bin i r d) = 
    cBin i r d (recAB cNil cBin i) (recAB cNil cBin d)

-- II
esNil :: AB a -> Bool
esNil arbol = case arbol of
    (Nil) -> True
    (Bin i r d) -> False

esNil2 :: AB a -> Bool
esNil2 = foldAB True (\ri r rd -> False)

altura :: AB a -> Int
altura = foldAB 0 (\ri _ rd -> 1 + max ri rd)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ri _ rd -> ri + 1 + rd) 

-- III
foldAB1 :: (a -> a -> a -> a) -> AB a -> a
foldAB1 fBin (Bin Nil r Nil) = r
foldAB1 fBin (Bin Nil r d) = fBin r r (foldAB1 fBin d)
foldAB1 fBin (Bin i r Nil) = fBin (foldAB1 fBin i) r r
foldAB1 fBin (Bin i r d) = fBin (foldAB1 fBin i) r (foldAB1 fBin d)

elMejorDado :: (a -> a -> Bool) -> a -> a -> a
elMejorDado p x y = if p x y then x else y

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB p = foldAB1 (\ri r rd -> 
                            if p ri rd 
                            then elMejorDado p r ri
                            else elMejorDado p r rd
    )

-- IV
esMayorA :: Ord a => a -> AB a -> Bool
esMayorA x = foldAB True (\ri r rd -> r > x && ri && rd)

esMenorOIgualA :: Ord a => a -> AB a -> Bool
esMenorOIgualA x = foldAB True (\ri r rd -> r <= x && ri && rd)

-- esABB :: Ord a => AB a -> Bool
-- esABB Nil = True
-- esABB (Bin Nil r Nil) = True
-- esABB (Bin Nil r d) = esABB d && esMayorA r d
-- esABB (Bin i r Nil) = esABB i && esMenorOIgualA r i
-- esABB (Bin i r d) = esABB i && esABB d && esMayorA r d && esMenorOIgualA r i

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\i r d ri rd -> ri && rd && esMayorA r d && esMenorOIgualA r i)

-- V
-- Para el II se usó foldAB por la sencillez de su esquema estructural
-- Para el III se usó foldAB1 tomando la misma idea de foldr1 para mejorSegun
-- Para el IV se usó recAB ya que era necesario operar con la estructura de los subarboles 


-- Ejercicio 14

-- I
sonListasIguales :: Eq a => [a] -> [a] -> Bool
sonListasIguales [] [] = True
sonListasIguales (x:xs) (y:ys) = x == y && sonListasIguales xs ys

consecutivos :: Eq a => [[a]] -> [[a]]
consecutivos [] = []
consecutivos [x] = []
consecutivos (x:xs) = if sonListasIguales x (head xs)
                        then x : consecutivos (tail xs) 
                        else consecutivos xs

ramas :: Eq a => AB a -> [[a]]
ramas = consecutivos . (foldAB [[]] (\ri r rd -> (map (r:) ri) ++ (map (r:) rd)))

-- ramas2 :: AB a -> [AB a]
-- ramas2 = recAB [Nil] (\i r d ri rd -> 
--             if esHoja (Bin i r d) 
--             then map (Bin ri r Nil) ri -- ???
--         )


esHoja :: AB a -> Bool
esHoja Nil = False
esHoja (Bin Nil r Nil) = True
esHoja (Bin i r d) = False

sumarHoja :: AB a -> Int
sumarHoja arbol = if esHoja arbol then 1 else 0

cantHojas :: AB a -> Int
cantHojas = recAB 0 (\d r i ri rd -> sumarHoja (Bin i r d) + rd + ri)

espejo :: AB a -> AB a
espejo = foldAB Nil (\ri r rd -> Bin rd r ri)

-- II
mismaEstructura :: AB a -> AB a -> Bool
mismaEstructura = foldAB (\ab -> esNil ab) 
            (\ri r rd (Bin i _ d) -> rd d && ri i)

-- igualLongitud :: [a] -> [a] -> Bool
-- igualLongitud x y = length x == length y
-- mismaEstructura :: Eq a => AB a -> AB a -> Bool
-- mismaEstructura ab ab2 = igualLongitud (ramas ab) (ramas ab2) && all (True==) (mapDoble igualLongitud (ramas ab) (ramas ab2))


-- Ejercicio 15
data AIH a = Hoja a | Arbol (AIH a) (AIH a)

-- a.
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fArbol (Hoja a) = fHoja a
foldAIH fHoja fArbol (Arbol i d) = 
    fArbol (foldAIH fHoja fArbol i) (foldAIH fHoja fArbol d)

-- b.
alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (const 1) (\ri rd -> 1 + max ri rd)

tamañoAIH :: AIH a -> Integer
tamañoAIH = foldAIH (const 1) (+)

-- c.
-- arbolesTipoUnit :: [AIH ()]
-- arbolesTipoUnit = Hoja () : [Arbol (last arbolesTipoUnit) (last arbolesTipoUnit)]

-- d. ??


-- Ejercicio 16

-- I
data RoseTree a = Rose a [RoseTree a]

-- II
foldRT :: (a -> b) -> (a -> [b] -> b) -> RoseTree a -> b
foldRT fRose fTree (Rose n []) = fRose n
foldRT fRose fTree (Rose n l) = fTree n (map (foldRT fRose fTree) l)

-- foldRT :: (a -> [b] -> b) -> RoseTree a -> b
-- foldRT fTree (Rose n l) = fTree n (map (foldRT fTree) l)

-- III
-- a.
hojas :: RoseTree a -> [a]
hojas = foldRT (:[]) (\n l -> n : concat l)

-- hojasRT = foldRT (\n l -> if null l
--                             then [n]
--                             else n : (concat l))

-- b.
distancias :: RoseTree a -> [Int]
distancias = foldRT (const [0]) (\n l -> 0 : map (+1) (concat l))

-- c.
alturaRT :: RoseTree a -> Int
alturaRT = (+1) . (mejorSegun (>)) . distancias

alturaRT2 :: RoseTree a -> Int
alturaRT2 = foldRT (const 1) (\n l -> 1 + mejorSegun (>) l)
