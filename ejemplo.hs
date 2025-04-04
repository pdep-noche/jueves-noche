siguiente :: Integer -> Integer
siguiente nro = nro + 1


calcular :: Integer -> Integer
calcular nro | even nro = siguiente nro
             | otherwise = doble nro

doble :: Integer -> Integer
doble nro = nro * 2


aproboAlumno :: Integer -> Bool
aproboAlumno nota = nota >= 6

-- segundo :: (Integer, Integer, Integer) -> Integer
segundo (_, elem, _) = elem


calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (primero, segundo) = (duplicaPar primero, sumarUnoImpar segundo)

duplicaPar :: Integer -> Integer
duplicaPar nro | even nro = doble nro
               | otherwise = nro


sumarUnoImpar :: Integer -> Integer
sumarUnoImpar nro | odd nro = siguiente nro
                  | otherwise = nro

-- III Declaratividad
and' :: Bool -> Bool -> Bool
and'   priBool segBool  | not priBool = False
                        | not segBool = False
                        | otherwise = True

-- II Declaratividad
and'' :: Bool -> Bool -> Bool
and'' priCond segCond | priCond = segCond
                      | otherwise = False

-- I Declaratividad
and''' :: Bool -> Bool -> Bool
and''' True segCond = segCond   --- Pattern Matching 
and''' _ _ = False

or' :: Bool -> Bool -> Bool
or' _ True = True
or' True _ = True
or' _ _ = False

or'' :: Bool-> Bool ->Bool
or'' False  segcond = segcond
or'' _ _ = True

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota1, nota2, nota3) =  nota1 `max` (nota2 `max` nota3)

notaMaxima' :: Alumno -> Nota
notaMaxima' (_, nota1, nota2, nota3) =  max nota1 (max nota2  nota3)

cuadruple :: Integer -> Integer
cuadruple nro = doble (doble nro)

esMayorA :: Integer -> Bool
esMayorA nro = doble (siguiente (nro + 2)) > 10


--triple
--(\x -> x * 3)

-- siguiente
--(\x -> x + 1)

-- suma 
--(\x y -> x + y)

-- sumar2
-- (\x -> x + 2)



