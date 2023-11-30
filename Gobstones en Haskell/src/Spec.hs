module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests del TP" $ do
    it "inicializarTablero Funciona" $ do
      inicializarTablero 3 3 `shouldBe` tableroVacio

    it "El cabezal se movio correctamente" $ do
      mover Derecha tableroDePrueba `shouldBe` tableroBordeDerecha

    it "La bolita se puso correctamente" $ do
      poner Roja tableroDePrueba `shouldBe` tableroDeTestPoner

    it "La bolita se saco correctamente" $ do
      sacar Roja tableroDeTestPoner `shouldBe` tableroDePrueba 

    it "Hay bolitas de ese color" $ do
      hayBolitas Roja tableroDeTestPoner `shouldBe` True 

    it "Las bolitas se contaron correctamente" $ do
      contarCantidadBolitas Roja tableroDeTestPoner `shouldBe` 1 

    it "Se fue al borde correctamente" $ do
      irAlBorde Derecha tableroDePrueba `shouldBe` tableroDeTestIrAlBorde

    it "Se verifico la posibilidad de moverse correctamente" $ do
      puedeMoverse Derecha tableroDeTestIrAlBorde `shouldBe` False 

    it "Alternativa Funciona" $ do
      alternativa (hayBolitas Roja) [mover Derecha, sacar Roja] [poner Roja, poner Verde] tableroDePrueba `shouldBe` tableroDeTestAlternativa

    it "Repetir Funciona" $ do
      repetir 2 [mover Arriba, poner Negra] tableroDePrueba `shouldBe` tableroDeTestRepetir

    it "Mientras Funciona" $ do
      mientras (puedeMoverse Arriba) [poner Roja, mover Derecha] tableroDePrueba `shouldBe` tableroDeTestMientras
