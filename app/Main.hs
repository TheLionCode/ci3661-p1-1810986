module Main where

import qualified Data.Map.Strict as Map
import Motor.Mundo (leerMundo)
import Motor.Tipos (Mundo (..), renderError)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( hGetContents'
  , hPutStrLn
  , hSetEncoding
  , stderr
  , stdout
  , utf8
  , withFile
  , IOMode (ReadMode)
  )
import System.IO.Error (catchIOError, ioeGetErrorString)

-- Punto de entrada provisional: solo reconoce el archivo.
-- El bucle de juego reemplaza este main más adelante.
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  argumentos <- getArgs
  case argumentos of
    [ruta] -> reconocerArchivo ruta
    _ -> do
      hPutStrLn stderr "Uso: aventura ARCHIVO"
      exitWith (ExitFailure 1)

reconocerArchivo :: FilePath -> IO ()
reconocerArchivo ruta = do
  leido <-
    catchIOError
      (Right <$> leerTexto ruta)
      (\problema -> pure (Left (ioeGetErrorString problema)))
  case leido of
    Left problema -> do
      putStrLn ("Error: no se pudo leer \"" ++ ruta ++ "\": " ++ problema)
      exitWith (ExitFailure 1)
    Right texto ->
      case leerMundo texto of
        Left errores -> do
          mapM_ (putStrLn . renderError) errores
          exitWith (ExitFailure 1)
        Right mundo -> do
          putStrLn (resumen mundo)
          exitWith ExitSuccess

leerTexto :: FilePath -> IO String
leerTexto ruta =
  withFile ruta ReadMode $ \handle -> do
    hSetEncoding handle utf8
    hGetContents' handle

resumen :: Mundo -> String
resumen mundo =
  "Mundo reconocido: "
    ++ mundoTitulo mundo
    ++ " ("
    ++ cifra (Map.size (mundoSalas mundo)) "sala" "salas"
    ++ ", "
    ++ cifra (Map.size (mundoObjetos mundo)) "objeto" "objetos"
    ++ ")"

cifra :: Int -> String -> String -> String
cifra n uno varios =
  show n ++ " " ++ if n == 1 then uno else varios
