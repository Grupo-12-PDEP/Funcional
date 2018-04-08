{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec 



-- Eventos

data Persona = Persona {
     nombre :: String,
     billetera :: Float
} deriving (Show) -- Para simplificar las cosas, armamos la data Persona que contiene nombre y billetera para usar con los eventos
-- y con los usuarios.
-- Los eventos devolverán a la persona, con su billetera modificada. Esto para poder aplicar eventos a otros eventos.

billeteraTest = Persona {nombre = "X", billetera = 10} -- Para testear

deposito (Persona nombre billetera) numero = (Persona nombre (billetera + numero))

upgrade (Persona nombre billetera) | billetera*0.2 <= 10 = (Persona nombre (billetera * 1.2))
                                   | otherwise = (Persona nombre billetera)

extraccion (Persona nombre billetera) numero | numero < 0 = error "Debe ser positivo!"
                                        | numero >= 0 && numero <= billetera = (Persona nombre (billetera-numero))
                                        | otherwise = (Persona nombre 0)

cierreCuenta (Persona nombre billetera) = (Persona nombre 0)

quedaIgual (Persona nombre billetera) = (Persona nombre billetera)



-- Usuarios

pepe = Persona {nombre = "Jose", billetera = 10}

lucho = Persona {nombre = "Luciano", billetera = 2}

pepe2 = Persona {nombre = "Jose2", billetera = 20}


-- Transacciones

transaccionUno (Persona nombre billetera) | nombre == "Luciano" = cierreCuenta (Persona nombre billetera) 
                                          | otherwise = quedaIgual (Persona nombre billetera)

transaccionDos (Persona nombre billetera) | nombre == "Jose" = deposito (Persona nombre billetera) 5
                                          | otherwise = upgrade (Persona nombre billetera)