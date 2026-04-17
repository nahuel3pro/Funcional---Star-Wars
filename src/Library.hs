module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

---- Pruebas
gerardo :: Persona
gerardo = UnaPersona "Gerardo" 1985
fiestaGerardo :: Fiesta
fiestaGerardo = UnaFiesta gerardo ["Nahuel", "Lucas", "Micaela", "Tomás", "Matías"] "rock"

type Nombre = String
type Apodo = String
type Genero = String
type Anio = Number
type Cantidad = Number
---- Estructuras

data Persona = UnaPersona{
 	nombre :: Nombre,
	anioNacimiento :: Anio
} deriving (Show)

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
esGrande fiesta = ((>=3).length.listaInvitados) fiesta

esCumplanieroMayorDeEdad :: Fiesta -> Bool
esCumplanieroMayorDeEdad fiesta = (2026 - (anioNacimiento.cumpleaniero) fiesta) >= 18

esAburrida :: Fiesta -> Bool
esAburrida fiesta = (generoMusica fiesta == "clasico") && ((not.esGrande) fiesta)

buenaFiesta :: Fiesta -> Bool
buenaFiesta fiesta = (tieneBuenaMusica fiesta) && (esGrande fiesta) && (esCumplanieroMayorDeEdad fiesta)

invitar :: Fiesta -> Persona -> Fiesta
invitar fiesta persona = UnaFiesta{
	cumpleaniero = cumpleaniero fiesta,
	listaInvitados = (nombre persona) : (listaInvitados fiesta),
	generoMusica = generoMusica fiesta
	}