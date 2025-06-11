data Postulante = UnPostulante {nombre :: String, edad :: Int, remuneracion :: Float, conocimientos :: [String]} | Estudiante {legajo :: String, conocimientos :: [String]}deriving Show 
 
pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto González" 20 12000.0 ["Haskell", "Php"]

julia :: Postulante
julia = Estudiante "2343453454" ["Haskell", "Wollok"]


type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"

type Requisito = Postulante -> Bool
---1 a
tieneConocimientos :: Puesto -> Requisito
tieneConocimientos puesto postulante = (all(\unConoReque -> elem unConoReque.conocimientos $ postulante) .conocimientoRequeridos) puesto

{-
ghci> tieneConocimientos jefe pepe
True
-}

edadAceptable :: Int -> Int -> Requisito
edadAceptable edadMin edadMax postulante = edadMin <= edad postulante && edadMax >= edad postulante

{-ghci> edadAceptable 18 60 pepe
True
-}

sinArreglo :: Requisito
sinArreglo postulante =  (apellidoDueno /=).last.words.nombre $ postulante


preselección :: [Postulante] -> [Requisito] -> [Postulante]
preselección postulantes requisitos = filter (cumpleTodosRequi requisitos)  postulantes
 

cumpleTodosRequi :: [Requisito] ->Postulante -> Bool
cumpleTodosRequi requisitos postulante = all (\requi-> requi postulante) requisitos

cumpleTodosRequi' :: [Requisito] ->Postulante -> Bool
cumpleTodosRequi' requisitos postulante = all ($ postulante) requisitos


---- 2 a
{-ghci> preselección [pepe, tito]  [(edadAceptable 30 40),tieneConocimientos jefe , sinArreglo]
[UnPostulante {nombre = "Jose Perez", edad = 35, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
--}

{-
preselección [pepe, tito]  [(edadAceptable 30 40),tieneConocimientos jefe , sinArreglo, (all (/= "repetir logica"). conocimientos)]
[UnPostulante {nombre = "Jose Perez", edad = 35, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}


---3 a
pasoAño :: [Postulante] -> [Postulante]
pasoAño postulantes =  [ (aumentarSueldo 27 .incrementarEdad ) unPostulante    | unPostulante <- postulantes]


{-
ghci> pasoAño [pepe, tito]
[UnPostulante {nombre = "Jose Perez", edad = 36, remuneracion = 19050.0, conocimientos = ["Haskell","Prolog","Wollok","C"]},UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]}]
-}

incrementarEdad :: Postulante -> Postulante
incrementarEdad postulante = postulante {edad = edad postulante + 1}

aumentarSueldo :: Float -> Postulante -> Postulante
aumentarSueldo porcentaje postulante = postulante { remuneracion = sueldoActualizado porcentaje postulante}

sueldoActualizado :: Float -> Postulante -> Float
sueldoActualizado porcentaje postulante = remuneracion postulante + ((remuneracion postulante)* porcentaje/ 100)


pasoAño' :: [Postulante ] -> [Postulante]
pasoAño' postulantes = map (aumentarSueldo 27.incrementarEdad) postulantes

{- ghci> pasoAño' [pepe, tito]
[UnPostulante {nombre = "Jose Perez", edad = 36, remuneracion = 19050.0, conocimientos = ["Haskell","Prolog","Wollok","C"]},UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]}]
-}

infinitosPostulantes = cycle [tito]


infinito = pepe: infinito


capacitar :: Postulante -> String -> Postulante
capacitar (UnPostulante nombre edad remuneracion conocimientos) conocimiento = UnPostulante nombre edad remuneracion (agregarConocimiento conocimiento conocimientos)
capacitar (Estudiante legajo conocimientos) conocimiento = Estudiante legajo  ((agregarConocimiento conocimiento. init) conocimientos)

agregarConocimiento :: String -> [String] -> [String]
agregarConocimiento conocimiento conocimientos = conocimiento: conocimientos