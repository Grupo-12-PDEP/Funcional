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
      billetera :: Billetera,
      nombre :: Nombre
} deriving (Show, Eq, Ord)


nuevaBilletera nuevoMonto unUsuario = unUsuario {billetera = nuevoMonto}


pepe = Usuario {nombre = "Jose", billetera = 10}
lucho = Usuario {nombre = "Luciano", billetera = 2}
pepe2 = Usuario {nombre = "Jose", billetera = 20}


-- Transacciones

type Transaccion = Usuario -> Evento
--La transaccion la definimos como una funcion que recibe como parametro el
--usuario a quien se le aplica, y devuelve el evento que produce en su billetera

crearTransaccion :: Nombre -> Evento -> Transaccion
crearTransaccion unNombre unEvento aQuienLeAplica | unNombre == nombre aQuienLeAplica = unEvento
                                                  | otherwise = quedaIgual

--Para facilitar la creacion de transaccciones, esta funcion recibe el evento
--de la transaccion y el nombre de quien le ocurre, y devuelve una transaccion
--que luego se podra aplicar a distintos usuarios.


transaccionUno = crearTransaccion "Luciano" cierreCuenta
transaccionDos = crearTransaccion "Jose" (deposito 5)




--Nuevos eventos

tocoYMeVoy :: Evento
tocoYMeVoy = cierreCuenta . upgrade . deposito 15

ahorroErrante :: Evento
ahorroErrante = deposito 10 . upgrade . deposito 8 . extraccion 1
  . deposito 2 . deposito 1


transaccionTres = crearTransaccion "Luciano" tocoYMeVoy
transaccionCuatro = crearTransaccion "Luciano" ahorroErrante




--Pago entre usuarios

crearPago :: Nombre -> Nombre -> Plata -> Transaccion
crearPago quienPaga quienRecibe monto aQuienLeAplica | nombre aQuienLeAplica == quienPaga = extraccion monto
                                                     | nombre aQuienLeAplica == quienRecibe = deposito monto
                                                     | otherwise = quedaIgual

--La funcion crearPago como parametros el monto y los involucrados en la
--transaccion, y devuelve una transaccion aplicable a cualquier usuarios

transaccionCinco = crearPago "Jose" "Luciano" 7




--Testing

testingPrimeraEntrega = hspec $ do
  describe "Tests de eventos sobre una billetera de 10 monedas" $ do
    it "1. Depositar 10 más. Debería quedar con 20 monedas."
      $ deposito 10 10 `shouldBe` 20
    it "2. Extraer 3: Debería quedar con 7."
      $ extraccion 3 10 `shouldBe` 7
    it "3. Extraer 15: Debería quedar con 0."
      $ extraccion 15 10 `shouldBe` 0
    it "4. Un upgrade: Debería quedar con 12."
      $ upgrade 10 `shouldBe` 12
    it "5. Cerrar la cuenta: 0."
      $ cierreCuenta 10 `shouldBe` 0
    it "6. Queda igual: 10."
      $ quedaIgual 10 `shouldBe` 10
    it "7. Depositar 1000, y luego tener un upgrade: 1020."
      $ (upgrade.(deposito 1000)) 10 `shouldBe` 1020
  describe "Tests sobre los usuarios definidos" $ do
    it "8. ¿Cuál es la billetera de Pepe? Debería ser 10 monedas."
      $ billetera pepe `shouldBe` 10
    it "9. ¿Cuál es la billetera de Pepe, luego de un cierre de su cuenta? Debería ser 0."
      $ (cierreCuenta.billetera) pepe `shouldBe` 0
    it "10. ¿Cómo quedaría la billetera de Pepe si le depositan 15 monedas, extrae 2, y tiene un Upgrade? Debería quedar en 27.6."
      $ (upgrade.(extraccion 2).(deposito 15).billetera) pepe `shouldBe` 27.6
  describe "Tests sobre transacciones" $ do
    it "11. Aplicar la transacción 1 a Pepe. Esto debería producir el evento quedaIgual, que si se aplicara a una billetera de 20 monedas, deberá dar una billetera con ese mismo monto."
      $ (transaccionUno pepe) 20 `shouldBe` 20
    it "12. Hacer que la transacción 2 se aplique a Pepe. El resultado, deberá ser el evento de depositar 5 monedas. Aplicarlo a una billetera de 10 monedas, mostrando que queda con 15."
      $ (transaccionDos pepe) 10 `shouldBe` 15
    it "13. Hacer que la transacción 2 se aplique al nuevo Pepe. Aplicar el evento resultante a una billetera de 50 monedas, y verificar que aumenta quedando con 55."
      $ (transaccionDos pepe2) 50 `shouldBe` 55
  describe "Tests sobre nuevos eventos" $ do
    it "14. Aplicar la transaccion 3 a Lucho. Aplicar el evento resultante a una billetera de 10 monedas. Debe quedar con 0."
      $ transaccionTres lucho 10 `shouldBe` 0
    it "15. Aplicar la transaccion 4 a Lucho. Aplicar el evento resultante a una billetera de 10 monedas. Debe quedar con 34."
      $ transaccionCuatro lucho 10 `shouldBe` 34
    it "16. Aplicar la transacción 5 a Pepe. Debería causar el evento de extracción de 7 unidades. Al aplicarlo a una billetera de 10 monedas, debería dar una nueva billetera de 3."
      $ transaccionCinco pepe 10 `shouldBe` 3
    it "17. Aplicar la transacción 5 a Lucho. Debería causar el evento de depósito de 7 unidades. Al aplicarlo a una billetera de 10 monedas, quedando con 17."
      $ transaccionCinco lucho 10 `shouldBe` 17





--Usuario luego de transacción

impactar :: Transaccion -> Usuario -> Usuario
impactar unaTransaccion unUsuario = nuevaBilletera ( (unaTransaccion unUsuario) (billetera unUsuario) ) unUsuario


--Bloque

type Bloque = [Transaccion]
bloque1 = [transaccionUno, transaccionDos, transaccionDos, transaccionDos,
  transaccionTres, transaccionCuatro, transaccionCinco, transaccionTres]

impactarBloque :: Bloque -> Usuario -> Usuario
impactarBloque = flip (foldr impactar)


quienesQuedanConBilleteraMayorA :: Plata -> Bloque -> [Usuario] -> [Usuario]
quienesQuedanConBilleteraMayorA nCreditos unBloque = filter (\ unUsuario -> billetera (impactarBloque unBloque unUsuario) > nCreditos )

comparoYMeQuedoConElOriginalDelMayor :: Bloque -> Usuario -> Usuario -> Usuario
comparoYMeQuedoConElOriginalDelMayor unBloque unUsuario otroUsuario | impactarBloque unBloque unUsuario > impactarBloque unBloque otroUsuario = unUsuario
                                                                    | otherwise = otroUsuario
quienEsElMasAdineradoConBloque :: Bloque -> [Usuario] -> Usuario
quienEsElMasAdineradoConBloque unBloque usuarios = foldr (comparoYMeQuedoConElOriginalDelMayor unBloque) (Usuario 0 "Nadie tiene un mango") usuarios

comparoYMeQuedoConElOriginalDelMenor :: Bloque -> Usuario -> Usuario -> Usuario
comparoYMeQuedoConElOriginalDelMenor unBloque unUsuario otroUsuario | impactarBloque unBloque unUsuario < impactarBloque unBloque otroUsuario = unUsuario
                                                                    | otherwise = otroUsuario
quienEsElMenosAdineradoConBloque :: Bloque -> [Usuario] -> Usuario
quienEsElMenosAdineradoConBloque unBloque usuarios = foldr (comparoYMeQuedoConElOriginalDelMenor unBloque) (Usuario 0 "Nadie tiene un mango") usuarios












testingSegundaEntrega = hspec $ do
  describe "Tests sobre usuarios luego de impactar transacciones" $ do
    it "18. Impactar la transacción 1 a Pepe. Debería quedar igual que como está inicialmente."
      $ impactar transaccionUno pepe `shouldBe` pepe
    it "19. Impactar la transacción 5 a Lucho. Debería producir que Lucho tenga 9 monedas en su billetera."
      $ impactar transaccionCinco lucho `shouldBe` nuevaBilletera 9 lucho
    it "20. Impactar la transacción 5 y luego la 2 a Pepe. Eso hace que tenga 8 en su billetera."
      $ (impactar transaccionDos . impactar transaccionCinco) pepe  `shouldBe` nuevaBilletera 8 pepe
  describe "Tests sobre bloques" $ do
    it "21. A partir del bloque 1 y pepe, decir cómo queda el usuario con su nuevo saldo en su billetera. Debería quedar con una billetera de 18."
      $ impactarBloque bloque1 pepe `shouldBe` Usuario 18 "Jose"
    it "22. Para el bloque 1, y los usuarios Pepe y Lucho, el único que quedaría con un saldo mayor a 10, es Pepe."
      $ quienesQuedanConBilleteraMayorA 10 bloque1 [pepe, lucho] `shouldBe` [pepe]
    it "23. Determinar quién sería el más adinerado con el bloque1. Pepe sería el más adinerado."
      $ quienEsElMasAdineradoConBloque bloque1 [pepe, lucho] `shouldBe` pepe
    it "24. Determinar quién sería el menos adinerado. Siguiendo el caso anterior, Lucho sería el menos adinerado."
      $ quienEsElMenosAdineradoConBloque bloque1 [pepe, lucho] `shouldBe` lucho






















--Fin
