{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec 



-- Para simplificar las cosas, armamos la data Evento que contiene nombre y billetera para usar con los eventos
-- y con los usuarios. De yapa está el campo Evento que tiene el nombre del Evento ocurrido. 

data User = User {
     nombre :: String,
     billetera :: Float,
     evento :: String -- evento devolverán el nombre del Evento
} deriving (Show, Eq)


-- Definimos el tipo Evento, y las funciones devolverán Eventos con estructura User
type Evento = User
deposito :: Float -> User -> Evento
upgrade :: User -> Evento
extraccion :: Float -> User -> Evento
cierreCuenta :: User -> Evento
quedaIgual :: User -> Evento
tocoYMeVoy :: User -> Evento
ahorranteErrante :: User -> Evento

--                                                         ENTREGA I



-- Vamos a crear billeteras acorde a los testeos que se pide, para evitar poner tanto código.

billeteraDiez = User {nombre = "Test", billetera = 10, evento = "Testeo"} -- Para testear
billeteraVeinte = User {nombre = "Test", billetera = 20, evento = "Testeo"} -- Para testear
billeteraCincuenta = User {nombre = "Test", billetera = 50, evento = "Testeo"} -- Para testear



{-
Eventos.
A continuación creamos funciones que devuelvan un tipo Evento. Se decidio devolver
toda la información del usuario, luego de aplicar el evento.
-}

deposito numero usuario | numero >= 0 = usuario {billetera = billetera(usuario)+numero, evento = "Deposito"}
                        | otherwise = error "La plata es positiva, de lo contrario es una extraccion."

upgrade usuario | billetera(usuario)*0.2 <= 10 = usuario {billetera = billetera(usuario)*1.2, evento = "Upgrade"}
                | otherwise = usuario {billetera = billetera(usuario)+10, evento = "Upgrade"}

extraccion numero usuario | numero < 0 = error "Debe ser positivo."
                          | numero >= 0 && numero <= billetera(usuario) = usuario {billetera = billetera(usuario) -numero, evento = "Extraccion"}
                          | otherwise = usuario {billetera = 0, evento = "Extraccion"}

cierreCuenta usuario = usuario {billetera = 0.0, evento = "Cierre de Cuenta"}

quedaIgual usuario = usuario {evento = "Queda Igual"}

tocoYMeVoy  usuario = usuario {billetera = ((billetera.cierreCuenta.upgrade.deposito 15) usuario), evento = "Toco y Me Voy"}

ahorranteErrante usuario = usuario {billetera = ((((billetera.deposito 10).(upgrade.deposito 8).(extraccion 1).(deposito 2).(deposito 1)) usuario)), evento = "Ahorrante Errante"}



{-
Enlistamos los usuarios que se utilizarán en la etapa de testeo,
como así también algunos extras para tener más casos de prueba.
-}

pepe = User {nombre = "Jose", billetera = 10, evento = "Soy Usuario"}

lucho = User {nombre = "Luciano", billetera = 2, evento = "Soy Usuario"}

pepe2 = User {nombre = "Jose", billetera = 20, evento = "Soy Usuario"}

-- Para ampliar nuestra base de Usuarios
rick = User {nombre = "Rick", billetera = 50, evento = "Soy Usuario"} 

morty = User {nombre = "Morty", billetera = 5, evento = "Soy Usuario"}

sunday = User {nombre = "Sunday", billetera = 123, evento = "Soy Usuario"}



{-
Creamos las transacciones, delegando la acción correspondiente a 
los eventos definidos anteriormente.
-}

transaccionUno usuario | nombre(usuario) == "Luciano" = cierreCuenta 
                       | otherwise = quedaIgual 

transaccionDos usuario | nombre(usuario) == "Jose" = deposito 5 
                       | otherwise = quedaIgual         
                       
transaccionTres usuario | nombre(usuario) == "Luciano" = tocoYMeVoy
                        | otherwise = quedaIgual

transaccionCuatro usuario | nombre(usuario) == "Luciano" = ahorranteErrante
                          | otherwise = quedaIgual

transaccionCinco usuario | nombre(usuario) == "Jose" = extraccion 7 -- Esta función recibe un usuario, el número a depositar/extraer y el otro usuario
                         | otherwise = deposito 7




-- Se realizan los testeos pedidos, ordenados según el TP.


test = hspec $ do
     describe "Testeo 1. Depositar 10 monedas a una billetera de 10 monedas" $ do
       it "Billetera con 20 monedas" $ (billetera.deposito 10) billeteraDiez `shouldBe` 20
       
     describe "Testeo 2. Extraer 3 monedas a una billetera de 10 monedas" $ do
       it "Billetera con 7 monedas" $ (billetera.extraccion 3) billeteraDiez `shouldBe` 7
       
     describe "Testeo 3. Extraer 15 monedas a una billetera de 10 monedas" $ do
       it "Billetera con 0 monedas" $ (billetera.extraccion 15) billeteraDiez `shouldBe` 0
       
     describe "Testeo 4. Hacer un upgrade a una billetera de 10 monedas" $ do
       it "Billetera con 12 monedas" $ (billetera.upgrade) billeteraDiez `shouldBe` 12
       
     describe "Testep 5. Cerrar la cuenta de una billetera de 10 monedas" $ do
       it "Billetera con 0 monedas" $ (billetera.cierreCuenta) billeteraDiez `shouldBe` 0
       
     describe "Testeo 6. No modificar una billetera de 10 monedas" $ do
       it "La billetera queda igual" $ (billetera.quedaIgual) billeteraDiez `shouldBe` 10
       
     describe "Testeo 7.Depostiar 1000 y hacer un upgrade, en una billetera de 10 monedas" $ do
       it "Billetera con 1020 monedas" $ (billetera.upgrade.deposito 1000) billeteraDiez `shouldBe` 1020
       
     describe "Testeo 8. Billetera de Pepe" $ do
       it "Deberia tener 10 monedas" $ billetera pepe `shouldBe` 10
       
     describe "Testeo 9. Billetera de Pepe, despues de cerrar su cuenta" $ do
       it "Deberia tener 0 monedas" $ (billetera.cierreCuenta) pepe `shouldBe` 0
       
     describe "Testeo 10. Billetera de Pepe si le depositan 15 monedas, extrae 2, y tiene un Upgrade" $ do
       it "Tiene 27.6 monedas" $ (((billetera.upgrade).(extraccion 2).deposito 15)pepe) `shouldBe` 27.6

     describe "Testeo 11. Aplicar la transaccion uno a Pepe" $ do
       it "Una billetera de 20 monedas deberia quedar con 20" $ (billetera.transaccionUno pepe) billeteraVeinte `shouldBe` 20
       
     describe "Testeo 12. Aplicar transaccion dos a Pepe" $ do
       it "Una billetera de 10 monedas deberia quedar con 15" $ (billetera.transaccionDos pepe) billeteraDiez `shouldBe` 15
       
     describe "Testeo 13. Aplicar transaccion dos a Pepe2" $ do
       it "Una billetera de 50 monedas deberia quedar con 55" $ (billetera.transaccionDos pepe2) billeteraCincuenta `shouldBe` 55
       
     describe "Testeo 14. Aplicar transaccion tres a billetera con 10 monedas" $ do
       it "Una billetera de 10 monedas deberia quedar con 0" $ (billetera.transaccionTres lucho) billeteraDiez `shouldBe` 0
       
     describe "Testeo 15. Aplicar transaccion cuatro a billetera con 10 monedas" $ do
       it "Una billetera de 10 monedas deberia quedar con 34" $ (billetera.transaccionCuatro lucho) billeteraDiez `shouldBe` 34
       
     describe "Testeo 16. Aplicar transaccion cinco a Pepe" $ do
       it "Una billetera de 10 monedas deberia quedar con 3" $ (billetera.transaccionCinco pepe) billeteraDiez `shouldBe` 3
       
     describe "Testeo 17. Aplicar transaccion cinco a Lucho" $ do
       it "Una billetera de 10 monedas deberia quedar con 17" $ (billetera.transaccionCinco lucho) billeteraDiez `shouldBe` 17
       
{-       -- Estos tests corresponden a la "función legible de transacciones"
     describe "Testeo 18. Aplicar la transaccion uno a Pepe" $ do
       it "La billetera de Pepe deberia quedar igual" $ (billetera.aplicar "transaccion uno" pepe) pepe `shouldBe` billetera(pepe)
     describe "Testeo 19. Aplicar transaccion cinco a Lucho" $ do
       it "La billetera de Lucho deberia quedar con 9 monedas" $ (billetera.aplicar "transaccion cinco" lucho) lucho `shouldBe` 9

       -}



--                                                         ENTREGA II



-- Generamos un método legible para ver el impacto de una transacción

aplicar transaccion  | transaccion == "transaccion uno" = transaccionUno 
                     | transaccion == "transaccion dos" = transaccionDos
                     | transaccion == "transaccion tres" = transaccionTres
                     | transaccion == "transaccion cuatro" = transaccionCuatro
                     | transaccion == "transaccion cinco" = transaccionCinco



-- Bloque

data Bloque = Bloque {
     bloque :: String,
     transacciones :: [User]
} deriving(Show,Eq)


--bloque_uno = Bloque {bloque = "Bloque 1", transacciones=[transaccionUno, transaccionDos, transaccionDos, transaccionDos, transaccionTres, transaccionCuatro, transaccionCinco, transaccionTres]}






















