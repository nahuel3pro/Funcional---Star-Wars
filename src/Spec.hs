module Spec where
import PdePreludat
import Library
import Test.Hspec

---- Pruebas
jorge :: Persona
jorge = UnaPersona "Jorge" 2006
gerardo :: Persona
gerardo = UnaPersona "Gerardo" 1985
fiestaGerardo :: Fiesta
fiestaGerardo = UnaFiesta gerardo ["Nahuel", "Lucas", "Micaela", "Tomás", "Matías"] "rock"
fiestaJorge :: Fiesta
fiestaJorge = UnaFiesta jorge ["Ludmila", "Pablo", "Micaela", "Tomás", "Matías", " ", " ", "", "", "", ""] "clasico"

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
      tieneBuenaMusica fiestaJorge `shouldBe` False
