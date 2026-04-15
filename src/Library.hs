module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

---- Pruebas
gerardo :: Persona
gerardo = UnaPersona "Gerardo" "Facha" 35
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
	apodo :: Apodo,
	anioNacimiento :: Anio
}

data Fiesta = UnaFiesta{
	cumpleaniero :: Persona,
	listaInv :: [Nombre],
	generoMusica :: Genero
}

---- Funciones
esMayor :: Persona -> Persona -> Bool
esMayor persona persona2 = anioNacimiento persona < anioNacimiento persona2

tieneBuenaMusica :: Fiesta -> Bool
tieneBuenaMusica fiesta = ((generoMusica fiesta) == "rock") || ((generoMusica fiesta) == "regueton viejo")

esGrande :: Fiesta -> Bool
esGrande fiesta = ((>=3).length.listaInv) fiesta

esCumplanieroMayorDeEdad :: Fiesta -> Bool
esCumplanieroMayorDeEdad fiesta = (edad.cumpleaniero) fiesta >= 18

esAburrida :: Fiesta -> Bool
esAburrida fiesta = (generoMusica fiesta == "clasico") && ((not.esGrande) fiesta)

buenaFiesta :: Fiesta -> Bool
buenaFiesta fiesta = (tieneBuenaMusica fiesta) && (esGrande fiesta) && (esCumplanieroMayorDeEdad fiesta)
