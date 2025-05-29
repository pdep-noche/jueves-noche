
{-
Aplicatión Parcial 
Currificación

(+ 7)

max 9

Composición de Funciones

  (not.even.(*8).(9+))

    (>8)
    (8>)

 Tipos Propios

 data Bool = True | False
-}

data Figura = Rectangulo { base:: Double, altura :: Double} | Circulo { radio :: Double}

area :: Figura -> Double
area (Circulo ra) = pi * ra^2
area (Rectangulo b h) = b * h

circulo :: Figura
circulo = Circulo 4

rectangulo :: Figura
rectangulo = Rectangulo 8 6

sayHello :: String -> String
sayHello alguien = "Hello " ++ alguien ++ "!"

duplicaPares numeros = [num *2 | num <- numeros , even num]

find' :: (a -> Bool) -> [a] -> a
find' f lista = (head.filter f) lista


data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edad :: Int } deriving Show 

politicos :: [Politico]
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]


{-
ghci> find' ((<50).edad) politicos
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}
-}

{-
ghci> find'((>3).length.proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}
-}

{-
ghci> find' (any ((>3).length.words).proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}
-}

type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}

promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos listaAlumnos = map (\(Alumno nombre notas) -> (nombre, promedio notas)) listaAlumnos


promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)


{-
ghci> promediosAlumnos[(Alumno "juan" [8,6]), (Alumno "maria" [7,9,4]), (Alumno "ana" [6,2,4])]
[("juan",7),("maria",6),("ana",4)]
-}


promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos listaNotas = map (promedio.filter(>=6))  listaNotas

{-
ghci> promediosSinAplazos [[6,8], [6,6,4]]
[7,6]
-}

aprobo :: Persona -> Bool
aprobo alumno =  (all (>=6).notas) alumno

{-
Ok, one module loaded.
ghci> aprobo (Alumno "manuel" [8, 6, 10, 9])
True

ghci> aprobo (Alumno "manuel" [8, 6, 2, 4]) 
False
-}