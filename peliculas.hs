import Text.Show.Functions
data Pelicula = Pelicula {nombrePelicula :: String, genero:: String, duracion:: Int, origen:: String} deriving (Show, Eq)

data Usuario = Usuario {nombre:: String, categoria :: String, edad:: Int, paisResidencia:: String, peliculasVistas :: [Pelicula], estadoSalud:: Int} deriving Show

psicosis = Pelicula "Psicosis" "Terror" 109 "Estados Unidos"
perfumeDeMujer :: Pelicula
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 95 "Iran"
lasTortugasTambienVuelan :: Pelicula
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"

juan :: Usuario
juan = Usuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer, elSaborDeLasCervezas] 60


----1
ver :: Pelicula -> Usuario -> Usuario
ver pelicula usuario =
     usuario { peliculasVistas = peliculasVistas usuario ++ [pelicula]}


{-
ghci> ver psicosis juan
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombrePelicula = "Psicosis", genero = "Terror", duracion = 109, origen = "Estados Unidos"}], estadoSalud = 60}
-}

----3
premiarInterFieles :: [Usuario]-> [Usuario]
premiarInterFieles usuarios = map  premiarUsuarioFiel usuarios

premiarUsuarioFiel  :: Usuario -> Usuario
premiarUsuarioFiel usuario | cumpleCondiciones usuario = subirCategoria usuario
                           | otherwise = usuario


cumpleCondiciones :: Usuario -> Bool
cumpleCondiciones usuario = (>=1) . length. peliculasQueNoSean "Estados Unidos" .peliculasVistas $ usuario

subirCategoria :: Usuario -> Usuario
subirCategoria usuario = usuario {categoria = nuevaCategoria.categoria $ usuario}

peliculasQueNoSean :: String -> [Pelicula] -> [Pelicula]
peliculasQueNoSean pais peliculas = filter ((pais /=).origen) peliculas

nuevaCategoria :: String -> String
nuevaCategoria "basica" = "estandar"
nuevaCategoria   _     = "premium"

{--
ghci> premiarInterFieles [juan]
[Usuario {nombre = "juan", categoria = "premium", edad = 23, paisResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombrePelicula = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = 60}]
--}

type Criterio = Pelicula -> Bool

teQuedasteCorto :: Criterio
teQuedasteCorto pelicula = (<35).duracion $ pelicula

cuestionDeGenero :: [String] -> Criterio
cuestionDeGenero generos pelicula = any (==(genero pelicula))  generos

deDondeSaliste :: String -> Criterio
deDondeSaliste unOrigen pelicula =  (== unOrigen).origen $ pelicula

vaPorEseLado :: (Eq a) => Pelicula ->(Pelicula -> a) -> Criterio
vaPorEseLado pelicula caracteristica otraPelicula =
    caracteristica pelicula == caracteristica otraPelicula


busqueda :: Usuario -> [Criterio] -> [Pelicula] -> [Pelicula]
busqueda usuario criterios peliculas = take 3 . filter (esRecomendable usuario criterios) $ peliculas


esRecomendable :: Usuario -> [Criterio] ->Pelicula -> Bool
esRecomendable usuario criterios pelicula = (not. vio pelicula) usuario && cumpleCriterios pelicula criterios


vio :: Pelicula -> Usuario -> Bool
vio pelicula usuario = elem pelicula . peliculasVistas $ usuario


cumpleCriterios :: Pelicula -> [Criterio] -> Bool
cumpleCriterios pelicula criterios = all ($ pelicula) criterios


{--
busqueda juan [cuestionDeGenero ["Drama", "Comedia"], deDondeSaliste "Iran", (not.teQuedasteCorto)] [psicosis, perfumeDeMujer, elSaborDeLasCervezas]
[]--}


data Capitulo = Capitulo { nombreCapitulo ::String,  generoCapitulo:: String, duracionCapitulo:: Int, origenCapitulo :: String,  afecta :: Usuario -> Usuario } deriving Show

consumeSeries :: Usuario -> Capitulo -> Usuario
consumeSeries usuario capitulo = (afecta capitulo) usuario


capitulo :: Capitulo
capitulo = Capitulo "capitulo1" "terror" 40 "Argentina" (\usuario -> usuario{estadoSalud = (estadoSalud usuario) - 15})
 
 {-
 hci> consumeSeries juan capitulo                                                                         
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombrePelicula = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = 45}
-}


maraton :: Usuario -> [Capitulo] -> Usuario
maraton usuario capitulos = foldl consumeSeries usuario capitulos


{--ghci> maraton juan [capitulo] 
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombrePelicula = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = 45}
--}

serieInfinita :: [Capitulo]
serieInfinita = repeat capitulo

---6

{--
ghci> maraton juan (take 5 serieInfinita)
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombrePelicula = "El sabor de las cervezas", genero = "Drama", duracion = 95, origen = "Iran"}], estadoSalud = -15}
--}



