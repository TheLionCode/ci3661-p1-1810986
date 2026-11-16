module Main where

import qualified Data.Map.Strict as Map
import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (ejecutar, estadoInicial)
import Motor.Persistencia (cargarPartida, guardarPartida)
import Motor.Tipos
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( IOMode (ReadMode)
  , hFlush
  , hGetContents'
  , hPutStrLn
  , hSetEncoding
  , isEOF
  , stderr
  , stdin
  , stdout
  , utf8
  , withFile
  )
import System.IO.Error (catchIOError, ioeGetErrorString)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  argumentos <- getArgs
  case argumentos of
    ["--validar", ruta] -> validar ruta
    [ruta] -> jugar ruta
    _ -> do
      hPutStrLn stderr "Uso: aventura ARCHIVO"
      hPutStrLn stderr "     aventura --validar ARCHIVO"
      exitWith (ExitFailure 1)

validar :: FilePath -> IO ()
validar ruta = do
  leido <- leerArchivo ruta
  case leido of
    Left problema -> fallarLectura ruta problema
    Right texto ->
      case leerMundo texto of
        Left errores -> rechazar errores
        Right _ -> do
          putStrLn "Mundo válido."
          exitWith ExitSuccess

jugar :: FilePath -> IO ()
jugar ruta = do
  leido <- leerArchivo ruta
  case leido of
    Left problema -> fallarLectura ruta problema
    Right texto ->
      case leerMundo texto of
        Left errores -> rechazar errores
        Right mundo -> arrancar mundo

arrancar :: Mundo -> IO ()
arrancar mundo = do
  let est = estadoInicial mundo
  putStrLn (mundoTitulo mundo)
  putStrLn (descripcion est)
  case estFin est of
    Just Gano -> do
      putStrLn "*** ¡Has ganado! ***"
      exitWith ExitSuccess
    _ -> bucle est

bucle :: Estado -> IO ()
bucle est = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then exitWith ExitSuccess
    else do
      linea <- getLine
      est' <- atender linea est
      case estFin est' of
        Just _ -> exitWith ExitSuccess
        Nothing -> bucle est'

atender :: String -> Estado -> IO Estado
atender linea est =
  case leerComando linea of
    Nothing -> do
      putStrLn "No entiendo ese comando."
      pure est
    Just (Guardar ruta) -> do
      resultado <- guardarPartida ruta est
      putStrLn $
        case resultado of
          Right () -> "Partida guardada."
          Left mensaje -> mensaje
      pure est
    Just (Cargar ruta) -> do
      resultado <- cargarPartida ruta est
      case resultado of
        Left mensaje -> do
          putStrLn mensaje
          pure est
        Right est' -> do
          putStrLn "Partida cargada."
          putStrLn (descripcion est')
          case estFin est' of
            Just Gano -> putStrLn "*** ¡Has ganado! ***"
            Just Tiempo -> putStrLn "*** Se acabó el tiempo ***"
            _ -> pure ()
          pure est'
    Just comando -> do
      let (mensajes, est') = ejecutar comando est
      mapM_ putStrLn mensajes
      pure est'

descripcion :: Estado -> String
descripcion est =
  case Map.lookup (estSala est) (mundoSalas (estMundo est)) of
    Just sala -> salaDesc sala
    Nothing -> "No estás en ninguna sala."

rechazar :: [ErrorMundo] -> IO ()
rechazar errores = do
  mapM_ (putStrLn . renderError) errores
  exitWith (ExitFailure 1)

fallarLectura :: FilePath -> String -> IO ()
fallarLectura ruta problema = do
  putStrLn ("Error: no se pudo leer \"" ++ ruta ++ "\": " ++ problema)
  exitWith (ExitFailure 1)

leerArchivo :: FilePath -> IO (Either String String)
leerArchivo ruta =
  catchIOError
    (Right <$> contenido ruta)
    (\problema -> pure (Left (ioeGetErrorString problema)))

contenido :: FilePath -> IO String
contenido ruta =
  withFile ruta ReadMode $ \handle -> do
    hSetEncoding handle utf8
    hGetContents' handle
