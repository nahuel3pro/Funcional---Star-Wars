module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

type Nombre = String
type Durabilidad = Number
type Escudo = Number
type Ataque = Number
type Poder = Nave -> Nave
type Flota = [Nave]

data Nave = UnaNave{
	nombre :: Nombre,
	durabilidad :: Durabilidad,
	escudo :: Escudo,
	ataque :: Ataque,
	poder :: Poder
} deriving(Show, Eq)

turbo :: Poder
turbo = modificarAtaque (+25)

reparacionEmergencia :: Poder
reparacionEmergencia = modificarDurabilidad (+50) . modificarAtaque (subtract 30)

superTurbo :: Poder
superTurbo = turbo . turbo . turbo . modificarDurabilidad (subtract 45)
 
tieFighter = UnaNave{
	nombre = "TIE Fighter",
	durabilidad = 200,
	escudo = 100,
	ataque = 50,
	poder = turbo

}
millennium = UnaNave{
	nombre = "Millennium Falcon",
	durabilidad = 1000,
	escudo = 500,
	ataque = 50,
	poder = reparacionEmergencia.modificarEscudo (+100)
}

-- Funciones poderes
modificarAtaque:: (Number->Number) -> Poder
modificarAtaque funcion nave = nave{
	ataque = funcion.ataque$nave
}

modificarDurabilidad:: (Number->Number) -> Poder
modificarDurabilidad funcion nave = nave{
	durabilidad = funcion.durabilidad $ nave
}

modificarEscudo :: (Number->Number) -> Poder
modificarEscudo funcion nave = nave{
	escudo = funcion.escudo $ nave
}

-- 2
durabilidadTotal :: Flota -> Number
durabilidadTotal = foldr1 (+).map (durabilidad)

-- 3
usarPoder :: Poder
usarPoder nave = poder nave nave 

danioRecibido :: Nave -> Nave -> Number
danioRecibido naveAtacante naveAtacada = max 0 . (subtract (escudo naveAtacada)) . ataque $ naveAtacante

resultadoAtaqueaNave :: Nave -> Nave -> Nave
resultadoAtaqueaNave naveAtacante naveAtacada = atacar (usarPoder naveAtacante) (usarPoder naveAtacada)

atacar :: Nave -> Nave -> Nave
atacar naveAtacante naveAtacada = modificarDurabilidad (subtract.danioRecibido naveAtacante $ naveAtacada) naveAtacada

-- 4
estaFueradeCombate :: Nave -> Bool
estaFueradeCombate = (==0).durabilidad

-- 5
-- Pasar por cada elemento de la lista y ver si cumple con la estrategia, de no cumplir, no se hace nada.
esNaveDebil :: Nave -> Bool
esNaveDebil = (<200).escudo

esPeligrosa :: Number -> Nave -> Bool
esPeligrosa valor = (> valor).ataque

quedariaFueraDeCombateContra :: Nave -> Nave -> Bool
quedariaFueraDeCombateContra naveAtacante naveAtacada = estaFueradeCombate . resultadoAtaqueaNave naveAtacante $ naveAtacada

type Estrategia = Nave -> Bool
realizarMision :: Estrategia -> Nave -> Flota -> Flota
realizarMision estrategia naveAtacante flota = map (atacarSiCumple estrategia naveAtacante) flota

atacarSiCumple :: Estrategia -> Nave -> Nave -> Nave
atacarSiCumple estrategia naveAtacante naveAtacada 
	| estrategia naveAtacada = resultadoAtaqueaNave naveAtacante naveAtacada
	| otherwise = naveAtacada

-- 6 
estrategiaMasEfectiva :: Estrategia -> Estrategia -> Nave -> Flota -> Estrategia
estrategiaMasEfectiva estrategia1 estrategia2 naveAtacante flota
	| durabilidadTotal (realizarMision estrategia1 naveAtacante flota) < durabilidadTotal (realizarMision estrategia2 naveAtacante flota) = estrategia1
	| otherwise = estrategia2

llevarMisionMasEfectiva :: Estrategia -> Estrategia -> Nave -> Flota -> Flota
llevarMisionMasEfectiva estrategia1 estrategia2 naveAtacante flota =
	realizarMision (estrategiaMasEfectiva estrategia1 estrategia2 naveAtacante flota) naveAtacante flota
