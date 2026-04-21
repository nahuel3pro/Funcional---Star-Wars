module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

type Nombre = String
type Apodo = String
type Genero = String
type Anio = Number
type Cantidad = Number
---- 

data Persona = UnaPersona{
	nombre :: Nombre,
	anioNacimiento :: Anio
}deriving(Show,Eq)

data Fiesta = UnaFiesta{
	cumpleaniero :: Persona,
	listaInvitados :: [Nombre],
	generoMusica :: Genero
}deriving(Show,Eq)

---- Funciones

--punto 2
ponerApodo :: Apodo -> Persona -> Persona
ponerApodo apodo persona = UnaPersona {
	nombre = apodo,
	anioNacimiento = anioNacimiento persona
}


-- Punto 1
esMayor :: Persona -> Persona -> Bool
esMayor persona persona2 = anioNacimiento persona < anioNacimiento persona2

-- punto 4
tieneBuenaMusica :: Fiesta -> Bool
tieneBuenaMusica fiesta = ((generoMusica fiesta) == "rock") || ((generoMusica fiesta) == "regueton viejo")

-- punto 3
esGrande :: Fiesta -> Bool
esGrande fiesta = ((>=10).length.listaInvitados) fiesta

esCumplanieroMayorDeEdad :: Fiesta -> Bool
esCumplanieroMayorDeEdad fiesta = (2026 - (anioNacimiento.cumpleaniero) fiesta) >= 18

--punto 7
esAburrida :: Fiesta -> Bool
esAburrida fiesta = (generoMusica fiesta == "clasico") && ((not.esGrande) fiesta)


--punto 8
buenaFiesta :: Fiesta -> Bool
buenaFiesta fiesta = (tieneBuenaMusica fiesta) && (esGrande fiesta) && (esCumplanieroMayorDeEdad fiesta)

--punto 6 
invitar :: Fiesta -> Persona -> Fiesta
invitar fiesta persona = fiesta{listaInvitados = (nombre persona) : (listaInvitados fiesta)}

-- punto 5
laMayor :: Persona -> Persona -> Persona
laMayor persona persona2
    | esMayor persona persona2 = persona
    | otherwise = persona2


-- punto 9
fingirDemencia :: Persona -> Fiesta -> Bool
fingirDemencia persona = buenaFiesta . agregarSiNoEsta persona

agregarSiNoEsta :: Persona -> Fiesta -> Fiesta
agregarSiNoEsta persona fiesta
    | elem (nombre persona) (listaInvitados fiesta) = fiesta
    | otherwise = invitar fiesta persona