module Spec where
import PdePreludat
import Library
import Test.Hspec

---- Pruebas
jorge :: Persona
jorge = UnaPersona "Jorge" 2015
gerardo :: Persona
gerardo = UnaPersona "Gerardo" 1985
ana :: Persona
ana = UnaPersona "Ana" 1950
fiestaGerardo :: Fiesta
fiestaGerardo = UnaFiesta gerardo ["Nahuel", "Lucas", "Micaela", "Tomás", "Matías"] "rock"
fiestaJorge :: Fiesta
fiestaJorge = UnaFiesta jorge ["Ludmila", "Pablo", "Micaela", "Tomás", "Matías", " ", " ", "", "", "", ""] "regueton viejo"
fiestaAna :: Fiesta
fiestaAna = UnaFiesta ana ["Ludmila", "Pablo"] "clasico"

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
  describe "Tests de persona" $ do
    it "Función es mayor" $ do
      esMayor gerardo jorge `shouldBe` True
      flip esMayor gerardo jorge `shouldBe` False
    it "Función la mayor de dos personas" $ do
      laMayor gerardo jorge `shouldBe` gerardo
    it "Función poner apodo" $ do
      ponerApodo "Fiera" gerardo `shouldBe` UnaPersona "Fiera" (anioNacimiento gerardo)
  describe "Tests de fiesta" $ do
    it "Función tiene buena música" $ do
      tieneBuenaMusica fiestaGerardo `shouldBe` True
      tieneBuenaMusica fiestaJorge `shouldBe` True
      tieneBuenaMusica fiestaAna  `shouldBe` False
    it "Función fiesta es aburrida" $ do
      esAburrida fiestaAna `shouldBe` True
      esAburrida fiestaGerardo `shouldBe` False
      esAburrida fiestaJorge `shouldBe` False
    it "El cumpleañero es mayor de edad" $ do
      esCumplanieroMayorDeEdad fiestaJorge `shouldBe` False
      esCumplanieroMayorDeEdad fiestaAna `shouldBe` True
