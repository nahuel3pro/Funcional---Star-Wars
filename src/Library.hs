module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Nombre = String
type Apodo = String
type Genero = String
type Anio = Number
type Cantidad = Number
---- Estructuras

data Persona = UnaPersona{
	nombre :: Nombre,
	anioNacimiento :: Anio
}

data Fiesta = UnaFiesta{
	cumpleaniero :: Persona,
	listaInvitados :: [Nombre],
	generoMusica :: Genero
}

---- Funciones
ponerApodo :: Apodo -> Persona -> Persona
ponerApodo apodo persona = UnaPersona {
	nombre = apodo,
	anioNacimiento = anioNacimiento persona
}

esMayor :: Persona -> Persona -> Bool
esMayor persona persona2 = anioNacimiento persona < anioNacimiento persona2

tieneBuenaMusica :: Fiesta -> Bool
tieneBuenaMusica fiesta = ((generoMusica fiesta) == "rock") || ((generoMusica fiesta) == "regueton viejo")

esGrande :: Fiesta -> Bool
esGrande fiesta = ((>=10).length.listaInvitados) fiesta

esCumplanieroMayorDeEdad :: Fiesta -> Bool
esCumplanieroMayorDeEdad fiesta = (2026 - (anioNacimiento.cumpleaniero) fiesta) >= 18

esAburrida :: Fiesta -> Bool
esAburrida fiesta = (generoMusica fiesta == "clasico") && ((not.esGrande) fiesta)

buenaFiesta :: Fiesta -> Bool
buenaFiesta fiesta = (tieneBuenaMusica fiesta) && (esGrande fiesta) && (esCumplanieroMayorDeEdad fiesta)

invitar :: Fiesta -> Persona -> Fiesta
invitar fiesta persona = fiesta{listaInvitados = (nombre persona) : (listaInvitados fiesta)}

laMayor :: Persona -> Persona -> Persona
laMayor persona persona2
    | esMayor persona persona2 = persona
    | otherwise = persona2

fingirDemencia :: Persona -> Fiesta -> Bool
fingirDemencia persona = buenaFiesta . agregarSiNoEsta persona

agregarSiNoEsta :: Persona -> Fiesta -> Fiesta
agregarSiNoEsta persona fiesta
    | elem (nombre persona) (listaInvitados fiesta) = fiesta
    | otherwise = invitar fiesta persona