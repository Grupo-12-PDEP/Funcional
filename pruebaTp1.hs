{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para m�todos de colecciones que no vienen por defecto (ver gu�a de lenguajes)
import Data.Maybe -- Por si llegan a usar un m�todo de colecci�n que devuelva �Just suElemento� o �Nothing�.
import Test.Hspec


--Eventos

type Billetera = Float
--Definimos el tipo Billetera como punto flotante para representar el dinero de
--la cuenta.


type Evento = Billetera -> Billetera
--Definimos el Evento como algo que acontece sobre una billetera y devuelve una
--nueva billetera con su valor afectado.

deposito :: Float -> Evento
deposito monto | monto > 0 = (+) monto
               | otherwise = error "el monto debe ser positivo"

upgrade :: Evento
upgrade unaBilletera | unaBilletera * 0.2 <= 10 = unaBilletera * 1.2
                     | otherwise = unaBilletera + 10

extraccion :: Float -> Evento
extraccion monto unaBilletera | monto < 0 = error "el monto debe ser positivo"
                              | unaBilletera - monto < 0 = 0
                              | otherwise = unaBilletera - monto
cierreCuenta :: Evento
cierreCuenta unaBilletera = 0

quedaIgual :: Evento
quedaIgual unaBilletera = unaBilletera



--Usuarios

data Usuario = Usuario {
     nombre :: String,
     billetera :: Billetera
} deriving (Show)



pepe = Usuario "Jose" 10

lucho = Usuario "Luciano" 2

pepe2 = Usuario "Jose" 20




-- Transacciones

type Transaccion = Usuario -> Evento
--La transaccion la definimos como una funcion que recibe como parametro el
--usuario a quien se le aplica, y devuelve el evento que produce en su billetera

crearTransaccion :: String -> Evento -> Transaccion
crearTransaccion unNombre unEvento aQuienLeAplica | unNombre == nombre aQuienLeAplica = unEvento
                                                  | otherwise = quedaIgual

--Para facilitar la creacion de transaccciones, esta funcion recibe el evento
--de la transaccion y el nombre de quien le ocurre, y devuelve una transaccion
--que luego se podra aplicar a distintos usuarios.


transaccionUno = crearTransaccion "Luciano" cierreCuenta
transaccionDos = crearTransaccion "Jose" (deposito 5)




--Nuevos eventos

tocoYMeVoy :: Evento
tocoYMeVoy = cierreCuenta . upgrade . (deposito 15)

ahorroErrante :: Evento
ahorroErrante = (deposito 10) . upgrade . (deposito 8) . (extraccion 1)
  . (deposito 2) . (deposito 1)


transaccionTres = crearTransaccion "Luciano" tocoYMeVoy
transaccionCuatro = crearTransaccion "Luciano" ahorroErrante




--Pago entre usuarios

crearPago :: String -> String -> Float -> Transaccion
crearPago quienPaga quienRecibe monto aQuienLeAplica | nombre aQuienLeAplica == quienPaga = extraccion monto
                                                     | nombre aQuienLeAplica == quienRecibe = deposito monto
                                                     | otherwise = quedaIgual

--La funcion crearPago como parametros el monto y los involucrados en la
--transaccion, y devuelve una transaccion aplicable a cualquier usuarios

transaccionCinco = crearPago "Jose" "Luciano" 5




--
