module Motor.Persistencia
  ( guardarPartida
  , cargarPartida
  ) where

import qualified Data.Map.Strict as Map
import Motor.Nucleo (estadoInicial)
import Motor.Reconocedor (identificador, reconocer)
import Motor.Tipos
import System.Directory (doesFileExist)
import System.FilePath (normalise)
import System.IO
  ( IOMode (..)
  , hGetContents'
  , hPutStr
  , hSetEncoding
  , utf8
  , withFile
  )
import System.IO.Error (catchIOError)

guardarPartida :: FilePath -> Estado -> IO (Either String ())
guardarPartida ruta est =
  catchIOError
    (escribir (normalise ruta) (textoPartida est) >> pure (Right ()))
    (\_ -> pure (Left "No se pudo guardar la partida."))

cargarPartida :: FilePath -> Estado -> IO (Either String Estado)
cargarPartida ruta est = do
  let archivo = normalise ruta
  existe <- catchIOError (doesFileExist archivo) (\_ -> pure False)
  if not existe
    then pure (Left "No se pudo cargar la partida: el archivo no existe.")
    else
      catchIOError
        (leerArchivo archivo >>= pure . interpretar est)
        (\_ -> pure (Left "No se pudo cargar la partida: el archivo está dañado."))

escribir :: FilePath -> String -> IO ()
escribir ruta texto =
  withFile ruta WriteMode $ \handle -> do
    hSetEncoding handle utf8
    hPutStr handle texto

leerArchivo :: FilePath -> IO String
leerArchivo ruta =
  withFile ruta ReadMode $ \handle -> do
    hSetEncoding handle utf8
    hGetContents' handle

textoPartida :: Estado -> String
textoPartida est =
  unlines
    ( ["MUNDO: " ++ mundoTitulo mundo, "SALA: " ++ textoId (estSala est)]
        ++ turnos
        ++ map (\ident -> "INVENTARIO: " ++ textoId ident) (estInventario est)
        ++ desplazados
    )
  where
    mundo = estMundo est
    turnos =
      case estTurnos est of
        Nothing -> []
        Just n -> ["TURNOS: " ++ show n]
    origen = ubicacionOriginal mundo
    desplazados =
      [ "EN: " ++ textoId objeto ++ " " ++ textoId sala
      | (objeto, sala) <- Map.toList (estUbicacionObjetos est)
      , Map.lookup objeto origen /= Just sala
      ]

interpretar :: Estado -> String -> Either String Estado
interpretar est texto =
  case foldl paso (Right vacio) (lines texto) of
    Left mensaje -> Left mensaje
    Right acum -> reconstruir (estMundo est) acum
  where
    paso (Left mensaje) _ = Left mensaje
    paso (Right acum) cruda =
      case trim cruda of
        "" -> Right acum
        linea ->
          case partirClave linea of
            Nothing -> Left danado
            Just (clave, valor) -> anotar acum clave valor

reconstruir :: Mundo -> Acum -> Either String Estado
reconstruir mundo acum =
  case pMundo acum of
    Nothing -> Left danado
    Just titulo
      | titulo /= mundoTitulo mundo -> Left otroMundo
      | otherwise ->
          seguir
  where
    seguir =
      case (pSala acum, turnosCoherentes mundo (pTurnos acum)) of
        (Just sala, Just limite)
          | Map.member sala (mundoSalas mundo) ->
              armar mundo sala limite (pInventario acum) (pMovidos acum)
        _ -> Left danado

armar :: Mundo -> Id -> Maybe Int -> [Id] -> [(Id, Id)] -> Either String Estado
armar mundo sala limite inventario movidos
  | inventarioMal mundo inventario = Left danado
  | movidosMal mundo inventario movidos = Left danado
  | otherwise =
      let base = estadoInicial mundo
          sinLlevados =
            foldl (\mapa ident -> Map.delete ident mapa) (estUbicacionObjetos base) inventario
          ubicados =
            foldl (\mapa (objeto, destino) -> Map.insert objeto destino mapa) sinLlevados movidos
          est =
            base
              { estSala = sala
              , estInventario = inventario
              , estUbicacionObjetos = ubicados
              , estTurnos = limite
              , estFin = Nothing
              }
       in Right
            ( if metaCumplida est
                then est { estFin = Just Gano }
                else est
            )

turnosCoherentes :: Mundo -> Maybe Int -> Maybe (Maybe Int)
turnosCoherentes mundo (Just n)
  | n < 0 = Nothing
  | mundoTurnos mundo == Nothing = Nothing
  | otherwise = Just (Just n)
turnosCoherentes mundo Nothing
  | mundoTurnos mundo == Nothing = Just Nothing
  | otherwise = Nothing

inventarioMal :: Mundo -> [Id] -> Bool
inventarioMal mundo inventario =
  any (not . definido mundo) inventario || repetidos inventario

movidosMal :: Mundo -> [Id] -> [(Id, Id)] -> Bool
movidosMal mundo inventario movidos =
  repetidos (map fst movidos)
    || any fuera movidos
  where
    fuera (objeto, sala) =
      objeto `elem` inventario
        || not (definido mundo objeto)
        || not (Map.member sala (mundoSalas mundo))

definido :: Mundo -> Id -> Bool
definido mundo ident = Map.member ident (mundoObjetos mundo)

repetidos :: Eq a => [a] -> Bool
repetidos [] = False
repetidos (x : xs) = x `elem` xs || repetidos xs

metaCumplida :: Estado -> Bool
metaCumplida est =
  case mundoMeta (estMundo est) of
    LlegarA sala -> estSala est == sala
    LlevarA objeto sala ->
      estSala est == sala && objeto `elem` estInventario est

data Acum = Acum
  { pMundo :: Maybe String
  , pSala :: Maybe Id
  , pTurnos :: Maybe Int
  , pInventario :: [Id]
  , pMovidos :: [(Id, Id)]
  }

vacio :: Acum
vacio = Acum Nothing Nothing Nothing [] []

anotar :: Acum -> String -> String -> Either String Acum
anotar acum "MUNDO" valor
  | pMundo acum == Nothing = Right acum { pMundo = Just valor }
  | otherwise = Left danado
anotar acum "SALA" valor
  | pSala acum == Nothing =
      case reconocer identificador valor of
        Just ident -> Right acum { pSala = Just ident }
        Nothing -> Left danado
  | otherwise = Left danado
anotar acum "TURNOS" valor
  | pTurnos acum == Nothing =
      case enteroNoNegativo valor of
        Just n -> Right acum { pTurnos = Just n }
        Nothing -> Left danado
  | otherwise = Left danado
anotar acum "INVENTARIO" valor =
  case reconocer identificador valor of
    Just ident -> Right acum { pInventario = pInventario acum ++ [ident] }
    Nothing -> Left danado
anotar acum "EN" valor =
  case words valor of
    [objeto, sala] ->
      case (reconocer identificador objeto, reconocer identificador sala) of
        (Just identObj, Just identSala) ->
          Right acum { pMovidos = pMovidos acum ++ [(identObj, identSala)] }
        _ -> Left danado
    _ -> Left danado
anotar _ _ _ = Left danado

ubicacionOriginal :: Mundo -> Map.Map Id Id
ubicacionOriginal mundo =
  Map.fromList
    [ (objeto, salaId sala)
    | sala <- Map.elems (mundoSalas mundo)
    , objeto <- salaObjetos sala
    ]

enteroNoNegativo :: String -> Maybe Int
enteroNoNegativo texto
  | null texto = Nothing
  | all esDigito texto =
      Just (foldl (\acc c -> acc * 10 + fromEnum c - fromEnum '0') 0 texto)
  | otherwise = Nothing

esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'

partirClave :: String -> Maybe (String, String)
partirClave linea =
  case break (== ':') linea of
    (cruda, ':' : resto)
      | claveValida clave -> Just (clave, trim resto)
      where
        clave = trim cruda
    _ -> Nothing

claveValida :: String -> Bool
claveValida clave =
  not (null clave) && all (\c -> c >= 'A' && c <= 'Z') clave

trim :: String -> String
trim = quitarFin . quitarInicio
  where
    quitarInicio = dropWhile esBlanco
    quitarFin = reverse . dropWhile esBlanco . reverse

esBlanco :: Char -> Bool
esBlanco c = c == ' ' || c == '\t' || c == '\r'

textoId :: Id -> String
textoId (Id texto) = texto

danado :: String
danado = "No se pudo cargar la partida: el archivo está dañado."

otroMundo :: String
otroMundo = "No se pudo cargar la partida: es de otro mundo."
