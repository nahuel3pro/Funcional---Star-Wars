module Library where
--import PdePreludat

type Nombre = String
type AnioNacimiento = Int
type EstiloMusica = String




-- modelo persona

data Persona = Persona {
    nombre :: String,
    anioNacimiento :: Int
} deriving (Eq, Show)

--modelo fiesta

data Fiesta = Fiesta {
    cumpleaniero :: Persona,
    invitados :: [Persona],
    estiloMusica :: String
} deriving (Eq, Show)

juan :: Persona
juan = Persona "Juan" 1995

esMayor :: Persona -> Persona -> Bool
esMayor persona persona2 = anioNacimiento persona < anioNacimiento persona2

ponerApodo :: Persona -> String -> Persona
ponerApodo persona apodo = persona { nombre = apodo }