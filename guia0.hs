-- Ejercicio 2
-- a.
valorAbsoluto :: Float -> Float
valorAbsoluto a | a >= 0 = a
                | otherwise = -a

-- b.
type Año = Int
bisiesto :: Año -> Bool
bisiesto a = a `mod` 4 == 0 

-- c.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) 

-- d.
esDivisor :: Int -> Int -> Bool
esDivisor n m = n `mod` m == 0

divisores :: Int -> [Int]
divisores n = filter (esDivisor n) [1..n]

esPrimo :: Int -> Bool
esPrimo n = divisores n == [1,n] 

divisoresPrimos :: Int -> [Int]
divisoresPrimos n = filter esPrimo (divisores n) 

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (divisoresPrimos n)

-- Ejercicio 3
-- a.
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1/n)

-- b.
aEntero :: Either Int Bool -> Int
aEntero (Right a) = if a then 1 else 0
aEntero (Left a) = a

-- Ejercicio 4
-- a.
limpiar :: String -> String -> String
limpiar a = filter (\x -> not (elem x a))

limpiar2 :: String -> String -> String
limpiar2 a = filter (not . flip elem a)

-- b.
flength :: [a] -> Float
flength [] = 0
flength (x:xs) = 1 + flength xs

prom :: [Float] -> Float
prom a = (sum a) / (flength a)

difPromedio :: [Float] -> [Float]
difPromedio a = map (\x -> x - prom a) a

-- c.
-- todosIguales :: [Int] -> Bool
-- todosIguales [] = True
-- todosIguales (x:xs) = esIgual x xs
