data Animal= Raton {nombre :: String, edad :: Double, peso :: Double, enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

modificarEdad :: (Double -> Double ) -> Animal -> Animal
modificarEdad f animal = animal {edad = (f.edad)animal}

{-
ghci> modificarEdad (2*) cerebro
Raton {nombre = "Cerebro", edad = 18.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
--}


modificarNombre :: ( String -> String ) -> Animal -> Animal
modificarNombre f animal = animal { nombre = (f.nombre)animal}

modificarPeso :: ( Double -> Double  ) -> Animal -> Animal
modificarPeso f animal = animal { peso = (f.peso)animal}

modificarEnfermedades :: ([String] ->[String] ) -> Animal -> Animal
modificarEnfermedades f animal = animal { enfermedades = f.enfermedades $ animal}


hierbaBuena :: Animal -> Animal
hierbaBuena raton = modificarEdad sqrt raton


hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad animal = modificarEnfermedades (filter (/=enfermedad))  animal

alcachofa :: Animal -> Animal
alcachofa animal = modificarPeso segunPeso animal

segunPeso :: Double -> Double
segunPeso peso | peso > 2 = peso * 0.9 
               | otherwise = peso * 0.95

hierbaMagica :: Animal -> Animal
hierbaMagica animal = modificarEdad (0*). modificarEnfermedades (const [])  $ animal


medicamento :: [(Animal-> Animal)] -> Animal -> Animal
medicamento hierbas animal = foldl (\animal hierba -> hierba animal)  animal hierbas

{-
ghci> medicamento [alcachofa, (hierbaVerde "brucelosis"), hierbaBuena] c
erebro 
Raton {nombre = "Cerebro", edad = 3.0, peso = 0.19, enfermedades = ["sarampi\243n","tuberculosis"]}}
-}

antiAge :: Animal -> Animal
antiAge animal = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) animal

{-
ghci> antiAge cerebro
Raton {nombre = "Cerebro", edad = 1.3160740129524924, peso = 0.19, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}

-}

reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia animal = medicamento ([hierbaVerde "obesidad"] ++ replicate potencia alcachofa) animal
 

 {-
 ghci> reduceFatFast 2 cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.1805, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa unAnimal = medicamento (map hierbaVerde  enfermedadesInfecciosas)  unAnimal


{-}
ghci> hierbaMilagrosa cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["sarampi\243n"]}

-}

cantidadIdeal f  =  head. filter f $ [1..]

estanMejoresQueNunca  :: [Animal] -> (Animal -> Animal) -> Bool
estanMejoresQueNunca animales medicamento = all ((<1).peso.medicamento) animales

orejudo = Raton "Orejudo" 4.0 8.1 ["sinusitis"]

{-
ghci> estanMejoresQueNunca [cerebro, orejudo] antiAge
False
-}

nuevoExperimento :: [Animal] -> Int
nuevoExperimento animales =cantidadIdeal (estanMejoresQueNunca animales.reduceFatFast)

{-
ghci> nuevoExperimento [cerebro, orejudo]
27
-}