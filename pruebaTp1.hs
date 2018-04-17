{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para m�todos de colecciones que no vienen por defecto (ver gu�a de lenguajes)
import Data.Maybe -- Por si llegan a usar un m�todo de colecci�n que devuelva �Just suElemento� o �Nothing�.
import Test.Hspec


--Eventos

type Plata = Float
type Billetera = Plata
--Definimos el tipo Billetera como Plata, y a su vez el tipo Plata como punto
--flotante para representar el dinero de la cuenta.


type Evento = Billetera -> Billetera
--Definimos el Evento como algo que acontece sobre una billetera y devuelve una
--nueva billetera con su valor afectado.

deposito :: Plata -> Evento
deposito = (+)

upgrade :: Evento
upgrade unaBilletera = (+) (min (unaBilletera*0.2) 10) unaBilletera

extraccion :: Plata -> Evento
extraccion monto unaBilletera = max 0 ((-) unaBilletera monto)

cierreCuenta :: Evento
cierreCuenta unaBilletera = 0

quedaIgual :: Evento
quedaIgual unaBilletera = unaBilletera



--Usuarios

type Nombre = String

data Usuario = Usuario {
     nombre :: Nombre,
     billetera :: Billetera
} deriving (Show)

pepe = Usuario {nombre = "Jose", billetera = 10}
lucho = Usuario {nombre = "Luciano", billetera = 2}
pepe2 = Usuario {nombre = "Jose", billetera = 20}


-- Transacciones

type Transaccion = Usuario -> Evento
--La transaccion la definimos como una funcion que recibe como parametro el
--usuario a quien se le aplica, y devuelve el evento que produce en su billetera

crearTransaccion :: Usuario -> Evento -> Transaccion
crearTransaccion unUsuario unEvento aQuienLeAplica | nombre unUsuario == nombre aQuienLeAplica = unEvento
                                                   | otherwise = quedaIgual

--Para facilitar la creacion de transaccciones, esta funcion recibe el evento
--de la transaccion y el nombre de quien le ocurre, y devuelve una transaccion
--que luego se podra aplicar a distintos usuarios.


transaccionUno = crearTransaccion lucho cierreCuenta
transaccionDos = crearTransaccion pepe (deposito 5)




--Nuevos eventos

tocoYMeVoy :: Evento
tocoYMeVoy = (cierreCuenta . upgrade . deposito 15)

ahorroErrante :: Evento
ahorroErrante = (deposito 10 . upgrade . deposito 8 . extraccion 1. deposito 2 . deposito 1)


transaccionTres = crearTransaccion lucho tocoYMeVoy
transaccionCuatro = crearTransaccion lucho ahorroErrante




--Pago entre usuarios

crearPago :: Usuario -> Usuario -> Plata -> Transaccion
crearPago quienPaga quienRecibe monto aQuienLeAplica | nombre aQuienLeAplica == nombre quienPaga = extraccion monto
                                                     | nombre aQuienLeAplica == nombre quienRecibe = deposito monto
                                                     | otherwise = quedaIgual

--La funcion crearPago como parametros el monto y los involucrados en la
--transaccion, y devuelve una transaccion aplicable a cualquier usuarios

transaccionCinco = crearPago pepe lucho 7




--Testing

testing = hspec $ do
  describe "Tests de eventos sobre una billetera de 10 monedas" $ do
    it "1. Depositar 10 más. Debería quedar con 20 monedas." $ deposito 10 10 `shouldBe` 20
    it "2. Extraer 3: Debería quedar con 7." $ extraccion 3 10 `shouldBe` 7
    it "3. Extraer 15: Debería quedar con 0." $ extraccion 15 10 `shouldBe` 0
    it "4. Un upgrade: Debería quedar con 12." $ upgrade 10 `shouldBe` 12
    it "5. Cerrar la cuenta: 0." $ cierreCuenta 10 `shouldBe` 0
    it "6. Queda igual: 10." $ quedaIgual 10 `shouldBe` 10
    it "7. Depositar 1000, y luego tener un upgrade: 1020." $ (upgrade.(deposito 1000)) 10 `shouldBe` 1020
  describe "Tests sobre los usuarios definidos" $ do
    it "8. ¿Cuál es la billetera de Pepe? Debería ser 10 monedas." $ billetera pepe `shouldBe` 10
    it "9. ¿Cuál es la billetera de Pepe, luego de un cierre de su cuenta? Debería ser 0." $ (cierreCuenta.billetera) pepe `shouldBe` 0
    it "10. ¿Cómo quedaría la billetera de Pepe si le depositan 15 monedas, extrae 2, y tiene un Upgrade? Debería quedar en 27.6." $ (upgrade.(extraccion 2).(deposito 15).billetera) pepe `shouldBe` 27.6
  describe "Tests sobre transacciones" $ do
    it "11. Aplicar la transacción 1 a Pepe. Esto debería producir el evento quedaIgual, que si se aplicara a una billetera de 20 monedas, deberá dar una billetera con ese mismo monto." $ (transaccionUno pepe) 20 `shouldBe` 20
    it "12. Hacer que la transacción 2 se aplique a Pepe. El resultado, deberá ser el evento de depositar 5 monedas. Aplicarlo a una billetera de 10 monedas, mostrando que queda con 15." $ (transaccionDos pepe) 10 `shouldBe` 15
    it "13. Hacer que la transacción 2 se aplique al nuevo Pepe. Aplicar el evento resultante a una billetera de 50 monedas, y verificar que aumenta quedando con 55." $ (transaccionDos pepe2) 50 `shouldBe` 55
  describe "Tests sobre nuevos eventos" $ do
    it "14. Aplicar la transaccion 3 a Lucho. Aplicar el evento resultante a una billetera de 10 monedas. Debe quedar con 0." $ transaccionTres lucho 10 `shouldBe` 0
    it "15. Aplicar la transaccion 4 a Lucho. Aplicar el evento resultante a una billetera de 10 monedas. Debe quedar con 34." $ transaccionCuatro lucho 10 `shouldBe` 34
    it "16. Aplicar la transacción 5 a Pepe. Debería causar el evento de extracción de 7 unidades. Al aplicarlo a una billetera de 10 monedas, debería dar una nueva billetera de 3." $ transaccionCinco pepe 10 `shouldBe` 3
    it "17. Aplicar la transacción 5 a Lucho. Debería causar el evento de depósito de 7 unidades. Al aplicarlo a una billetera de 10 monedas, quedando con 17." $ transaccionCinco lucho 10 `shouldBe` 17

--Fin
