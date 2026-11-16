module Main where

import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (ejecutar, estadoInicial)
import Motor.Persistencia (cargarPartida, guardarPartida)
import Motor.Tipos
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import Test.HUnit
  ( Counts (..)
  , Test (..)
  , assertBool
  , assertEqual
  , assertFailure
  , runTestTT
  , (~:)
  )

main :: IO ()
main = do
  conteo <- runTestTT pruebas
  if errors conteo + failures conteo > 0
    then exitFailure
    else pure ()

pruebas :: Test
pruebas =
  TestList
    [ "entrega tiene título" ~: tituloEntrega
    , "entrega tiene 9 salas" ~: salasEntrega
    , "entrega tiene 5 objetos" ~: objetosEntrega
    , "entrega pide llevar el proyecto" ~: metaEntrega
    , "entrega no limita turnos" ~: sinTurnosEntrega
    , "sintaxis inválida da 4 errores" ~: cuentaArchivo "mundos/invalido_sintaxis.txt" 4
    , "semántica inválida da 9 errores" ~: cuentaArchivo "mundos/invalido_semantica.txt" 9
    , "el guion del ejemplo gana" ~: guionGana
    , "sin carnet la biblioteca no abre" ~: salidaBloqueada
    , "un turno fallido agota el tiempo" ~: seAcabaElTiempo
    , "mirar no gasta turno" ~: mirarNoGasta
    , "ganar en el último turno no es derrota" ~: ganarEnElUltimo
    , "ir norte" ~: assertEqual "ir" (Just (Ir Norte)) (leerComando "ir norte")
    , "oeste suelto" ~: assertEqual "oeste" (Just (Ir Oeste)) (leerComando "  oeste  ")
    , "coger" ~: assertEqual "coger" (Just (Tomar (Id "memoria_usb"))) (leerComando "coger memoria_usb")
    , "inv" ~: assertEqual "inv" (Just Inventario) (leerComando "inv")
    , "mayúsculas no" ~: assertEqual "mayus" Nothing (leerComando "IR NORTE")
    , "tomar solo no" ~: assertEqual "tomar" Nothing (leerComando "tomar")
    , "TURNOS cero no vale" ~: turnosCero
    , "una partida recuerda sala e inventario" ~: partidaRedonda
    ]

tituloEntrega :: IO ()
tituloEntrega = do
  mundo <- exigirMundo "mundos/entrega.txt"
  assertEqual "título" "Entrega a última hora" (mundoTitulo mundo)

salasEntrega :: IO ()
salasEntrega = do
  mundo <- exigirMundo "mundos/entrega.txt"
  assertEqual "salas" 9 (Map.size (mundoSalas mundo))

objetosEntrega :: IO ()
objetosEntrega = do
  mundo <- exigirMundo "mundos/entrega.txt"
  assertEqual "objetos" 5 (Map.size (mundoObjetos mundo))

metaEntrega :: IO ()
metaEntrega = do
  mundo <- exigirMundo "mundos/entrega.txt"
  assertEqual "meta" (LlevarA (Id "proyecto_impreso") (Id "oficina_del_profesor")) (mundoMeta mundo)

sinTurnosEntrega :: IO ()
sinTurnosEntrega = do
  mundo <- exigirMundo "mundos/entrega.txt"
  assertEqual "turnos" Nothing (mundoTurnos mundo)

cuentaArchivo :: FilePath -> Int -> IO ()
cuentaArchivo ruta esperados = do
  texto <- leerRuta ruta
  case leerMundo texto of
    Left errores -> assertEqual ruta esperados (length errores)
    Right _ -> assertFailure (ruta ++ " debía ser inválido")

guionGana :: IO ()
guionGana = do
  mundo <- exigirMundo "mundos/entrega.txt"
  let lineas =
        [ "mirar"
        , "oeste"
        , "tomar carnet"
        , "inventario"
        , "norte"
        , "oeste"
        , "tomar memoria_usb"
        , "este"
        , "norte"
        , "arriba"
        , "este"
        , "tomar llave_del_ascensor"
        , "oeste"
        , "oeste"
        , "tomar proyecto_impreso"
        , "este"
        , "arriba"
        , "norte"
        ]
      (est, mensajes) = aplicarLineas (estadoInicial mundo) lineas
  assertEqual "sala" (Id "oficina_del_profesor") (estSala est)
  assertEqual "fin" (Just Gano) (estFin est)
  assertBool "victoria" (any (== "*** ¡Has ganado! ***") mensajes)

salidaBloqueada :: IO ()
salidaBloqueada = do
  mundo <- exigirMundo "mundos/entrega.txt"
  let est = (estadoInicial mundo) { estSala = Id "plaza" }
      (mensajes, est') = ejecutar (Ir Oeste) est
  assertEqual "sigue en la plaza" (Id "plaza") (estSala est')
  assertBool "pide el carnet" (any (contiene "carnet") mensajes)
  assertEqual "no termina" Nothing (estFin est')

seAcabaElTiempo :: IO ()
seAcabaElTiempo = do
  mundo <- mundoDe (pasilloConTurnos 1 ++ "META: llegar a b\n")
  let (mensajes, est) = ejecutar (Ir Norte) (estadoInicial mundo)
  assertEqual "tiempo" (Just Tiempo) (estFin est)
  assertEqual "sala" (Id "a") (estSala est)
  assertBool "aviso" (any (== "*** Se acabó el tiempo ***") mensajes)

mirarNoGasta :: IO ()
mirarNoGasta = do
  mundo <- mundoDe (pasilloConTurnos 2 ++ "META: llegar a b\n")
  let (mensajes, est) = ejecutar Mirar (estadoInicial mundo)
  assertEqual "quedan 2" (Just 2) (estTurnos est)
  assertBool "no anuncia turno" (not (any (contiene "Turnos restantes") mensajes))

ganarEnElUltimo :: IO ()
ganarEnElUltimo = do
  mundo <- mundoDe (pasilloConTurnos 1 ++ "META: llegar a b\n")
  let (mensajes, est) = ejecutar (Ir Este) (estadoInicial mundo)
  assertEqual "ganó" (Just Gano) (estFin est)
  assertBool "victoria" (any (== "*** ¡Has ganado! ***") mensajes)
  assertBool "no se acabó" (not (any (== "*** Se acabó el tiempo ***") mensajes))

turnosCero :: IO ()
turnosCero = do
  case leerMundo (pasilloConTurnos 0 ++ "META: llegar a a\n" ++ cuerpo) of
    Left errores ->
      assertBool "nombra TURNOS" (any (contiene "TURNOS") (map errorMensaje errores))
    Right _ -> assertFailure "TURNOS: 0 debía fallar"

partidaRedonda :: IO ()
partidaRedonda = do
  mundo <- exigirMundo "mundos/entrega.txt"
  let est0 = estadoInicial mundo
      (_, est1) = ejecutar (Tomar (Id "carnet")) est0
      (_, est2) = ejecutar (Ir Norte) est1
      (_, est3) = ejecutar (Soltar (Id "carnet")) est2
      ruta = "/tmp/aventura-partida-prueba.txt"
  _ <- catchIOError (removeFile ruta) (\_ -> pure ())
  guardado <- guardarPartida ruta est3
  assertEqual "guarda" (Right ()) guardado
  cargado <- cargarPartida ruta (estadoInicial mundo)
  case cargado of
    Left mensaje -> assertFailure mensaje
    Right est -> do
      assertEqual "sala" (Id "plaza") (estSala est)
      assertEqual "inventario" [] (estInventario est)
      assertEqual "carnet en la plaza" (Just (Id "plaza")) (Map.lookup (Id "carnet") (estUbicacionObjetos est))
  faltante <- cargarPartida "/tmp/aventura-partida-que-no-existe.txt" est0
  case faltante of
    Left _ -> pure ()
    Right _ -> assertFailure "un archivo ausente no se carga"
  _ <- catchIOError (removeFile ruta) (\_ -> pure ())
  pure ()

aplicarLineas :: Estado -> [String] -> (Estado, [String])
aplicarLineas est [] = (est, [])
aplicarLineas est (linea : resto) =
  case leerComando linea of
    Nothing -> (est, ["no entendí " ++ linea])
    Just comando ->
      let (mensajes, est') = ejecutar comando est
          (estFinal, despues) = aplicarLineas est' resto
       in (estFinal, mensajes ++ despues)

exigirMundo :: FilePath -> IO Mundo
exigirMundo ruta = do
  texto <- leerRuta ruta
  case leerMundo texto of
    Right mundo -> pure mundo
    Left errores -> do
      assertFailure (unlines (map renderError errores))
      fail "no llega"

mundoDe :: String -> IO Mundo
mundoDe texto =
  case leerMundo (texto ++ cuerpo) of
    Right mundo -> pure mundo
    Left errores -> do
      assertFailure (unlines (map renderError errores))
      fail "no llega"

pasilloConTurnos :: Int -> String
pasilloConTurnos n =
  "MUNDO: Prueba\nINICIO: a\nTURNOS: " ++ show n ++ "\n"

cuerpo :: String
cuerpo =
  unlines
    [ "---"
    , "SALA: a"
    , "DESC: Sala a."
    , "SALIDA: este -> b"
    , "---"
    , "SALA: b"
    , "DESC: Sala b."
    , "SALIDA: oeste -> a"
    ]

leerRuta :: FilePath -> IO String
leerRuta relativa = do
  encontrada <- buscar relativa
  readFile encontrada

buscar :: FilePath -> IO FilePath
buscar relativa = do
  let candidatos = take 8 (iterate (\ruta -> ".." </> ruta) relativa)
  existentes <- filterM doesFileExist candidatos
  case existentes of
    (ruta : _) -> pure ruta
    [] -> do
      assertFailure ("no está el archivo " ++ relativa)
      fail "no llega"

contiene :: String -> String -> Bool
contiene aguja pajar = buscarAg pajar
  where
    buscarAg [] = False
    buscarAg resto@(_ : cola) = esPrefijo aguja resto || buscarAg cola
    esPrefijo [] _ = True
    esPrefijo _ [] = False
    esPrefijo (a : as) (b : bs) = a == b && esPrefijo as bs
