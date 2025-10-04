-- PRIMERO DEFINICIONES DE TIPOS Y AUXILIARES
-- ******************************************************

data Bit = O | I deriving (Show, Eq)

type Binario = [Bit]

valorBit :: Bit -> Int
valorBit O = 0
valorBit I = 1

-- SECCIÓN BINARIOS
-- ******************************************************

-- 1. toDecimal :: Binario -> Int
-- Queremos convertir un número binario a su valor decimal (asumiendo el orden normal, no invertido).
toDecimal :: Binario -> Int
toDecimal bits = foldl (\acum bit -> acum * 2 + (valorBit bit)) 0 bits
-- 2. toBin :: Int -> Binario
-- Queremos que, dado un número en representación decimal, devuelva la representación binaria.
toBin :: Int -> Binario
toBin 0 = [O]
toBin n
  | n < 0     = error "Para función toBin solo acepte números no negativos."
  | otherwise = reverse (auxiliarToBin n)
  where
    -- Función auxiliar recursiva que genera los bits en orden inverso (menos significativo primero).
    auxiliarToBin 0 = []
    auxiliarToBin m = bit : auxiliarToBin cociente
      where
        -- Calcula el resto de la división (el bit actual)
        bit = if m `mod` 2 == 0 then O else I
        -- Calcula el cociente para la siguiente iteración
        cociente = m `div` 2
-- Función auxiliar para contar 1s (mausqueherramienta para después)
contarUnos :: [Bit] -> Int
contarUnos = length . filter (== I)

-- Función auxiliar para sumar 3 bits (a, b y el acarreo c)
-- Devuelve una tupla: (Suma, Acarreo)
sumaBits :: Bit -> Bit -> Bit -> (Bit, Bit)
sumaBits a b c =
  let total = contarUnos [a, b, c]
  in case total of
       0 -> (O, O) -- 0 + 0 + 0 = 0 (suma 0, acarreo 0)
       1 -> (I, O) -- 1 (suma 1, acarreo 0)
       2 -> (O, I) -- 2 (suma 0, acarreo 1)
       3 -> (I, I) -- 3 (suma 1, acarreo 1)

-- Función auxiliar que quita los ceros 'O' iniciales (bits más significativos a la izquierda)
quitarCerosIzquierda :: Binario -> Binario
quitarCerosIzquierda [] = []
quitarCerosIzquierda [O] = [O] -- Deja un solo cero si el resultado es 0.
quitarCerosIzquierda (O:xs) = quitarCerosIzquierda xs
quitarCerosIzquierda xs = xs

-- Función principal de suma, que invierte los binarios para sumarlos por el bit menos significativo, y luego invierte el resultado.
sumarConCarry :: Binario -> Binario -> Bit -> Binario
sumarConCarry [] [] O = []
sumarConCarry [] [] I = [I]
sumarConCarry (x:xs) [] c =
    let (s, carry) = sumaBits x O c
    in s : sumarConCarry xs [] carry
sumarConCarry [] (y:ys) c =
    let (s, carry) = sumaBits O y c
    in s : sumarConCarry [] ys carry
sumarConCarry (x:xs) (y:ys) c =
    let (s, carry) = sumaBits x y c
    in s : sumarConCarry xs ys carry

-- 3. suma :: Binario -> Binario -> Binario
suma :: Binario -> Binario -> Binario
suma b1 b2 = quitarCerosIzquierda (reverse $ sumarConCarry (reverse b1) (reverse b2) O)

-- ******************************************************
-- SECCIÓN LISTAS
-- ******************************************************

-- 1. palindromo :: [a] -> Bool
-- Requiere la restricción Eq para poder comparar los elementos.
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

-- 2. diferenciaSimetrica :: [a] -> [a] -> [a]
-- Requiere la restricción Eq para poder verificar la pertenencia a una lista (`elem`).
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = soloEnXs ++ soloEnYs
  where
    -- Elementos de xs que NO están en ys.
    soloEnXs = [x | x <- xs, not (x `elem` ys)]
    -- Elementos de ys que NO están en xs.
    soloEnYs = [y | y <- ys, not (y `elem` xs)]

-- 3. conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = resto ++ [x:ys | ys <- resto]
  where
    resto = conjuntoPotencia xs

-- ******************************************************
-- SECCIÓN LISTAS DE LONGITUD PAR
-- ******************************************************

-- Definición del sinónimo de tipo (requerido al inicio de la sección)
type ListaPar a b = [(a, b)]

-- 1. longitud :: ListaPar a b -> Int
longitud :: ListaPar a b -> Int
longitud xs = length xs * 2

-- 2. myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d
myMap f g xs = [(f a, g b) | (a, b) <- xs]

-- 3. sumaPares :: ListaPar a b -> (a,b)
-- Requiere que 'a' y 'b' puedan ser sumados (clase Num).
sumaPares :: (Num a, Num b) => ListaPar a b -> (a, b)
sumaPares xs = foldr sumar (0, 0) xs
  where
    -- Función auxiliar para foldr
    sumar (a, b) (acumA, acumB) = (a + acumA, b + acumB)

-- 4. myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter :: ((a, b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter f xs = [par | par <- xs, f par]