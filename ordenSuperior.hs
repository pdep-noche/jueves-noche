{-- foldl  (\sem f -> f sem)  2 [(3+), (*2), (5+)]
  15

  ghci> foldl  (flip ($))  2 [(3+), (*2), (5+)]
  15

  foldr  (\f sem -> f sem)  2 [(3+), (*2), (5+)] 
  17

  ghci> foldr  ($)  2 [(3+), (*2), (5+)]              
     17
--}


type Nombre  = String
type InversionInicial = Integer
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos :: [Proyecto]
proyectos = [Proy "red social de arte"  200000 ["ing. en sistemas", "contador"], Proy "restaurante" 50000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 10000 ["cocinero"] ]

redArte :: Proyecto
redArte = Proy "red social de arte"  200000 ["ing. en sistemas", "contador"]


maximoProySegun :: (Proyecto -> Int) ->  [Proyecto] -> Proyecto
maximoProySegun f (proyecto: proyectos) = foldl (maximoProy f)   proyecto proyectos


maximoProy :: (Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maximoProy f unProy otroProy | f unProy > f otroProy = unProy
                             | otherwise = otroProy


{-
ghci> maximoProySegun inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}

ghci> maximoProySegun (length.profesionales) proyectos
Proy {nombre = "restaurante", inversionInicial = 50000, profesionales = ["cocinero","adm. de empresas","contador"]}

ghci> maximoProySegun (length.words.nombre) proyectos
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}
--}

