{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva "Just suElemento" o "Nothing".
import Test.Hspec -- Para poder usar los tests que se piden más abajo (ponerlo luego de instalar hspec!!)

--Eventos

type Plata = Float
type Billetera = Plata

--Definimos el tipo Billetera como Plata, y a su vez el tipo Plata como punto flotante para representar el dinero de la cuenta.

type Evento = Billetera -> Billetera

--Definimos el Evento como algo que acontece sobre una billetera y devuelve una nueva billetera con su valor afectado.

veintePorcientoDe :: Evento
veintePorcientoDe = (*) 0.2

deposito :: Plata -> Evento
deposito = (+)

upgrade :: Evento
upgrade unaBilletera = (min . veintePorcientoDe) unaBilletera 10 + unaBilletera

extraccion :: Plata -> Evento
extraccion monto unaBilletera = max 0 (unaBilletera - monto)

cierreCuenta :: Evento
cierreCuenta unaBilletera = 0

quedaIgual :: Evento
quedaIgual = id

--Usuarios

type Nombre = String

data Usuario = Usuario {
      billetera :: Billetera,
      nombre :: Nombre
} deriving (Show, Eq, Ord)

nuevaBilletera :: Plata -> Usuario -> Usuario
nuevaBilletera nuevoMonto unUsuario = unUsuario {billetera = nuevoMonto}

pepe = Usuario {nombre = "Jose", billetera = 10}
lucho = Usuario {nombre = "Luciano", billetera = 2}
pepe2 = Usuario {nombre = "Jose", billetera = 20}

-- Transacciones

type Transaccion = Usuario -> Evento

--La transaccion la definimos como una funcion que recibe como parametro el usuario a quien se le aplica, y devuelve el evento que produce en su billetera

crearTransaccion :: Nombre -> Evento -> Transaccion
crearTransaccion unNombre unEvento aQuienLeAplica | unNombre == nombre aQuienLeAplica = unEvento
                                                  | otherwise = quedaIgual

--Para facilitar la creacion de transaccciones, esta funcion recibe el evento de la transaccion y el nombre de quien le ocurre, y devuelve una transaccion que luego se podra aplicar a distintos usuarios.

transaccionUno :: Transaccion
transaccionUno = crearTransaccion "Luciano" cierreCuenta

transaccionDos :: Transaccion
transaccionDos = crearTransaccion "Jose" (deposito 5)

--Nuevos eventos

tocoYMeVoy :: Evento
tocoYMeVoy = cierreCuenta . upgrade . deposito 15

ahorroErrante :: Evento
ahorroErrante = deposito 10 . upgrade . deposito 8 . extraccion 1 . deposito 2 . deposito 1

transaccionTres :: Transaccion
transaccionTres = crearTransaccion "Luciano" tocoYMeVoy

transaccionCuatro :: Transaccion
transaccionCuatro = crearTransaccion "Luciano" ahorroErrante

--Pago entre usuarios

crearPago :: Nombre -> Nombre -> Plata -> Transaccion
crearPago quienPaga quienRecibe monto aQuienLeAplica | nombre aQuienLeAplica == quienPaga = extraccion monto
                                                     | nombre aQuienLeAplica == quienRecibe = deposito monto
                                                     | otherwise = quedaIgual

--La funcion crearPago como parametros el monto y los involucrados en la transaccion, y devuelve una transaccion aplicable a cualquier usuarios

transaccionCinco :: Transaccion
transaccionCinco = crearPago "Jose" "Luciano" 7

--Testing

testingPrimeraEntrega = hspec $ do

  describe "Tests de eventos sobre una billetera de 10 monedas" $ do
    it "1. Depositar 10 más. Debería quedar con 20 monedas." $ deposito 10 10 `shouldBe` 20
    it "2. Extraer 3: Debería quedar con 7." $ extraccion 3 10 `shouldBe` 7
    it "3. Extraer 15: Debería quedar con 0." $ extraccion 15 10 `shouldBe` 0
    it "4. Un upgrade: Debería quedar con 12." $ upgrade 10 `shouldBe` 12
    it "5. Cerrar la cuenta: 0." $ cierreCuenta 10 `shouldBe` 0
    it "6. Queda igual: 10." $ quedaIgual 10 `shouldBe` 10
    it "7. Depositar 1000, y luego tener un upgrade: 1020." $ (upgrade . deposito 1000) 10 `shouldBe` 1020

  describe "Tests sobre los usuarios definidos" $ do
    it "8. ¿Cuál es la billetera de Pepe? Debería ser 10 monedas."
      $ billetera pepe `shouldBe` 10
    it "9. ¿Cuál es la billetera de Pepe, luego de un cierre de su cuenta? Debería ser 0."
      $ (cierreCuenta . billetera) pepe `shouldBe` 0
    it "10. ¿Cómo quedaría la billetera de Pepe si le depositan 15 monedas, extrae 2, y tiene un Upgrade? Debería quedar en 27.6."
      $ (upgrade . extraccion 2 . deposito 15 . billetera) pepe `shouldBe` 27.6

  describe "Tests sobre transacciones" $ do
    it "11. Aplicar la transacción 1 a Pepe. Esto debería producir el evento quedaIgual, que si se aplicara a una billetera de 20 monedas, deberá dar una billetera con ese mismo monto."
      $ transaccionUno pepe 20 `shouldBe` 20
    it "12. Hacer que la transacción 2 se aplique a Pepe. El resultado, deberá ser el evento de depositar 5 monedas. Aplicarlo a una billetera de 10 monedas, mostrando que queda con 15."
      $ transaccionDos pepe 10 `shouldBe` 15
    it "13. Hacer que la transacción 2 se aplique al nuevo Pepe. Aplicar el evento resultante a una billetera de 50 monedas, y verificar que aumenta quedando con 55."
      $ transaccionDos pepe2 50 `shouldBe` 55

  describe "Tests sobre nuevos eventos" $ do
    it "14. Aplicar la transaccion 3 a Lucho. Aplicar el evento resultante a una billetera de 10 monedas. Debe quedar con 0."
      $ transaccionTres lucho 10 `shouldBe` 0
    it "15. Aplicar la transaccion 4 a Lucho. Aplicar el evento resultante a una billetera de 10 monedas. Debe quedar con 34."
      $ transaccionCuatro lucho 10 `shouldBe` 34

  describe "Tests sobre transacciones entre usuarios" $ do
    it "16. Aplicar la transacción 5 a Pepe. Debería causar el evento de extracción de 7 unidades. Al aplicarlo a una billetera de 10 monedas, debería dar una nueva billetera de 3."
      $ transaccionCinco pepe 10 `shouldBe` 3
    it "17. Aplicar la transacción 5 a Lucho. Debería causar el evento de depósito de 7 unidades. Al aplicarlo a una billetera de 10 monedas, quedando con 17."
      $ transaccionCinco lucho 10 `shouldBe` 17




--                                                          ENTREGA II


--Testing

testingSegundaEntrega = hspec $ do

  describe "Tests de impacto" $ do
    it "Impactar la transaccion 1 a Pepe"
      $ impactar transaccionUno pepe `shouldBe` pepe
    it "Impactar la transaccion 5 a Lucho"
      $ impactar transaccionCinco lucho `shouldBe` lucho {billetera = 9}
    it "Impactar la transaccion 5 y luego la 2 a Pepe"
      $ (.) (impactar transaccionDos) (impactar transaccionCinco) pepe `shouldBe` pepe {billetera = 8}

  describe "Tests de bloque" $ do
    it "Aplicar bloque 1 a Pepe"
      $ impactarBloque bloque1 pepe `shouldBe` pepe {billetera = 18}
    it "Solo Pepe tendra un saldo mayor a 10 luego de aplicar el bloque 1"
      $ quienesQuedanConBilleteraMayorA 10 bloque1 [pepe, lucho] `shouldBe` [pepe]
    it "Pepe queda con mas dinero luego de aplicar el bloque 1"
      $ elMasRicoLuegoDe (impactarBloque bloque1) [pepe, lucho] `shouldBe` pepe
    it "Lucho queda con menos dinero luego de aplicar el bloque 1"
      $ elMasPobreLuegoDe (impactarBloque bloque1) [pepe, lucho] `shouldBe` lucho

  describe "Tests de block chain" $ do
    it "Para Pepe el peor bloque de la block chain fue el bloque 1"
      $ impactarBloque (peorBloque blockchain pepe) pepe `shouldBe` pepe {billetera = 18}
    it "Pepe luego de la block chain posee 115 creditos en su billetera"
      $ aplicarBlockchain blockchain pepe `shouldBe` pepe {billetera = 115}
    it "Pepe queda con 51 creditos si solo aplicamos los 3 primeros bloques"
      $ aplicarNBloques blockchain 3 pepe `shouldBe` pepe {billetera = 51}
    it "El saldo total entre Lucho y Pepe luego de un block chain es 115"
      $ (sum . map billetera) (aplicarBlockchainAVariosUsuarios blockchain [lucho, pepe]) `shouldBe` 115

  describe "Tests de block chain infinito" $ do
    it "Pepe pasa los 10000 creditos luego de los 11 primeros bloques"
      $ cuantosBloquesSeNecesitanParaTener 10000 blockInf pepe `shouldBe` 11



--Usuario luego de transacción

impactar :: Transaccion -> Usuario -> Usuario
impactar unaTransaccion unUsuario = nuevaBilletera (unaTransaccion unUsuario (billetera unUsuario)) unUsuario

--Bloque

type Bloque = [Transaccion]

bloque1 :: Bloque
bloque1 = [transaccionUno, transaccionDos, transaccionDos, transaccionDos, transaccionTres, transaccionCuatro, transaccionCinco, transaccionTres]

--Usuario luego de una cadena de transacciones seguidas

impactarBloque :: Bloque -> Usuario -> Usuario
impactarBloque = flip (foldr impactar)

--Funciones para comparar la billetera de un grupo de usuarios con un monto fijo

quienesQuedanConBilleteraMayorA :: Plata -> Bloque -> [Usuario] -> [Usuario]
quienesQuedanConBilleteraMayorA nCreditos unBloque = filter (\ unUsuario -> billetera (impactarBloque unBloque unUsuario) > nCreditos )

--Funciones para comparar las billeteras de un grupo de usuarios tras un bloque

type Impacto = Usuario -> Usuario

type CriterioSobreUsuarios = Usuario -> Usuario -> Bool

comparoResultadosYDevuelvoOriginal :: CriterioSobreUsuarios -> Impacto -> Usuario -> Usuario -> Usuario
comparoResultadosYDevuelvoOriginal unCriterio unImpacto unUsuario otroUsuario
  | unImpacto unUsuario `unCriterio` unImpacto otroUsuario = unUsuario
  | otherwise = otroUsuario

elMasRicoLuegoDe :: Impacto -> [Usuario] -> Usuario
elMasRicoLuegoDe unImpacto usuarios = foldr1 (comparoResultadosYDevuelvoOriginal (>) unImpacto) usuarios

elMasPobreLuegoDe :: Impacto -> [Usuario] -> Usuario
elMasPobreLuegoDe unImpacto usuarios = foldr1 (comparoResultadosYDevuelvoOriginal (<) unImpacto) usuarios

--BlockChain

type Blockchain = [Bloque]

bloque2 :: Bloque
bloque2 = [transaccionDos, transaccionDos, transaccionDos, transaccionDos, transaccionDos]

masBloque1 :: Blockchain
masBloque1 = repeat bloque1

blockchain :: Blockchain
blockchain = ( bloque2 : take 10 masBloque1 )

--Funciones que permiten aplicar cadenas de bloques a usuarios


comparoResultadosYDevuelvoBloqueOriginal :: CriterioSobreUsuarios -> Usuario -> Bloque -> Bloque -> Bloque
comparoResultadosYDevuelvoBloqueOriginal unCriterio unUsuario unBloque otroBloque
  | impactarBloque unBloque unUsuario `unCriterio` impactarBloque otroBloque unUsuario = unBloque
  | otherwise = otroBloque

peorBloque :: Blockchain -> Usuario -> Bloque
peorBloque unBlockChain unUsuario = foldr1 (comparoResultadosYDevuelvoBloqueOriginal (<) unUsuario) unBlockChain

aplicarBlockchain :: Blockchain -> Usuario -> Usuario
aplicarBlockchain = flip (foldr impactarBloque)

type CantidadBloques = Int

aplicarNBloques :: Blockchain -> CantidadBloques -> Usuario -> Usuario
aplicarNBloques unBlockchain cantidadBloques = aplicarBlockchain (take cantidadBloques unBlockchain)

aplicarBlockchainAVariosUsuarios :: Blockchain -> [Usuario] -> [Usuario]
aplicarBlockchainAVariosUsuarios unBlockchain = map (aplicarBlockchain unBlockchain)

--BlockChain infinito

generarBlockChainInf :: Bloque -> Blockchain
generarBlockChainInf bloqueSemilla = bloqueSemilla : generarBlockChainInf (bloqueSemilla ++ bloqueSemilla)

blockInf = generarBlockChainInf bloque1

cuantosBloquesSeNecesitanParaTener :: Plata -> Blockchain -> Usuario -> Int
cuantosBloquesSeNecesitanParaTener tantaPlata ( primerBloque : bloquesRestantes ) unUsuario
  | billetera unUsuario >= tantaPlata = 0
  | otherwise = 1 + cuantosBloquesSeNecesitanParaTener tantaPlata bloquesRestantes (impactarBloque primerBloque unUsuario)

cuantosBloquesSeNecesitanParaTener _ [] _ = error "es imposible llegar a ese monton con este BlockChain"








--Fin
