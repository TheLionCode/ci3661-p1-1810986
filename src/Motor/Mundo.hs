module Motor.Mundo
  ( leerMundo
  ) where

import Data.List (isInfixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import Motor.Reconocedor
  ( Reconocedor
  , cadena
  , direccion
  , exito
  , identificador
  , luego
  , reconocer
  , unoDe
  )
import Motor.Tipos

data Linea = Linea
  { numeroLinea :: Int
  , textoLinea :: String
  }

data Campo a
  = NoVisto
  | Invalido
  | Valido a

data Pieza
  = PiezaObjeto Objeto
  | PiezaSala Sala

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo texto =
  case partirBloques (preparar texto) of
    [] ->
      Left
        [ errorSinLinea "falta el campo MUNDO"
        , errorSinLinea "falta el campo INICIO"
        , errorSinLinea "falta el campo META"
        ]
    (cabecera : resto) ->
      let (erroresCabecera, cabeceraParseada) = parseCabecera cabecera
          bloques = map parseBloque resto
          errores = erroresCabecera ++ concatMap fst bloques
       in case (cabeceraParseada, errores) of
            (Just cab, []) ->
              Right (construir cab [pieza | (_, Just pieza) <- bloques])
            (_, []) ->
              Left [errorSinLinea "no se pudo leer la cabecera"]
            (_, errs) ->
              Left errs

construir :: (String, Id, Meta) -> [Pieza] -> Mundo
construir (titulo, inicio, meta) piezas =
  Mundo
    { mundoTitulo = titulo
    , mundoInicio = inicio
    , mundoMeta = meta
    , mundoObjetos = Map.fromList (elegir objetosDe)
    , mundoSalas = Map.fromList (elegir salasDe)
    }
  where
    objetosDe (PiezaObjeto objeto) = Just (objId objeto, objeto)
    objetosDe (PiezaSala _) = Nothing
    salasDe (PiezaSala sala) = Just (salaId sala, sala)
    salasDe (PiezaObjeto _) = Nothing
    elegir cual = [par | Just par <- map cual piezas]

preparar :: String -> [Linea]
preparar texto =
  filter conservar (zipWith empaquetar [1 ..] (lines texto))
  where
    empaquetar n cruda = Linea n (trim cruda)
    conservar (Linea _ ('#' : _)) = False
    conservar (Linea _ t) = not (null t)

trim :: String -> String
trim = quitarFin . quitarInicio
  where
    quitarInicio = dropWhile esBlanco
    quitarFin = reverse . dropWhile esBlanco . reverse

esBlanco :: Char -> Bool
esBlanco c = c == ' ' || c == '\t' || c == '\r'

partirBloques :: [Linea] -> [[Linea]]
partirBloques = filter (not . null) . agrupar []
  where
    agrupar acc [] = [reverse acc]
    agrupar acc (linea : resto)
      | textoLinea linea == "---" = reverse acc : agrupar [] resto
      | otherwise = agrupar (linea : acc) resto

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

errorEn :: Linea -> String -> ErrorMundo
errorEn linea mensaje = ErrorMundo (Just (numeroLinea linea)) mensaje

errorSinLinea :: String -> ErrorMundo
errorSinLinea = ErrorMundo Nothing

anclar :: [Linea] -> String -> ErrorMundo
anclar (primera : _) mensaje = errorEn primera mensaje
anclar [] mensaje = errorSinLinea mensaje

parseCabecera :: [Linea] -> ([ErrorMundo], Maybe (String, Id, Meta))
parseCabecera lineas = (errores, resultado)
  where
    (mundo, inicio, meta, errs) =
      foldl aplicar (NoVisto, NoVisto, NoVisto, []) lineas
    errores = reverse errs ++ faltas
    faltas =
      concat
        [ [anclar lineas "falta el campo MUNDO" | esNoVisto mundo]
        , [anclar lineas "falta el campo INICIO" | esNoVisto inicio]
        , [anclar lineas "falta el campo META" | esNoVisto meta]
        ]
    resultado =
      case (mundo, inicio, meta) of
        (Valido titulo, Valido ident, Valido metaValor)
          | null errores -> Just (titulo, ident, metaValor)
        _ -> Nothing

    aplicar (m, i, mt, es) linea =
      case partirClave (textoLinea linea) of
        Nothing ->
          (m, i, mt, errorEn linea "la línea no tiene la forma CLAVE: valor" : es)
        Just ("MUNDO", valor) ->
          case m of
            NoVisto -> (Valido valor, i, mt, es)
            _ -> (m, i, mt, errorEn linea "campo MUNDO repetido" : es)
        Just ("INICIO", valor) ->
          case i of
            NoVisto ->
              case reconocer identificador valor of
                Just ident -> (m, Valido ident, mt, es)
                Nothing ->
                  (m, Invalido, mt, errorEn linea (identInvalido valor) : es)
            _ -> (m, i, mt, errorEn linea "campo INICIO repetido" : es)
        Just ("META", valor) ->
          case mt of
            NoVisto ->
              case reconocer metaP valor of
                Just metaValor -> (m, i, Valido metaValor, es)
                Nothing ->
                  (m, i, Invalido, errorEn linea (clasificarMeta valor) : es)
            _ -> (m, i, mt, errorEn linea "campo META repetido" : es)
        Just (clave, _)
          | claveConocida clave ->
              (m, i, mt, errorEn linea (noPermitida clave "la cabecera") : es)
          | otherwise ->
              (m, i, mt, errorEn linea (claveDesconocida clave) : es)

parseBloque :: [Linea] -> ([ErrorMundo], Maybe Pieza)
parseBloque [] = ([errorSinLinea "bloque vacío"], Nothing)
parseBloque lineas@(primera : _) =
  case partirClave (textoLinea primera) of
    Just ("OBJETO", _) -> parseObjeto lineas
    Just ("SALA", _) -> parseSala lineas
    Just (clave, _) ->
      ( errorEn primera ("el bloque empieza por \"" ++ clave ++ "\" y no es OBJETO ni SALA")
          : erroresSueltos (tail lineas)
      , Nothing
      )
    Nothing ->
      ( errorEn primera "la línea no tiene la forma CLAVE: valor"
          : erroresSueltos (tail lineas)
      , Nothing
      )

parseObjeto :: [Linea] -> ([ErrorMundo], Maybe Pieza)
parseObjeto lineas = (errores, pieza)
  where
    (campoId, campoDesc, errs) =
      foldl aplicar (NoVisto, NoVisto, []) lineas
    errores = reverse errs ++ faltas
    faltas =
      concat
        [ [anclar lineas "falta el campo OBJETO" | esNoVisto campoId]
        , [anclar lineas "falta el campo DESC" | esNoVisto campoDesc]
        ]
    pieza =
      case (campoId, campoDesc) of
        (Valido ident, Valido desc) | null errores ->
          Just (PiezaObjeto (Objeto ident desc))
        _ -> Nothing

    aplicar (i, d, es) linea =
      case partirClave (textoLinea linea) of
        Nothing ->
          (i, d, errorEn linea "la línea no tiene la forma CLAVE: valor" : es)
        Just ("OBJETO", valor) ->
          case i of
            NoVisto ->
              case reconocer identificador valor of
                Just ident -> (Valido ident, d, es)
                Nothing -> (Invalido, d, errorEn linea (identInvalido valor) : es)
            _ -> (i, d, errorEn linea "campo OBJETO repetido" : es)
        Just ("DESC", valor) ->
          case d of
            NoVisto -> (i, Valido valor, es)
            _ -> (i, d, errorEn linea "campo DESC repetido" : es)
        Just (clave, _)
          | claveConocida clave ->
              (i, d, errorEn linea (noPermitida clave "un bloque de objeto") : es)
          | otherwise ->
              (i, d, errorEn linea (claveDesconocida clave) : es)

parseSala :: [Linea] -> ([ErrorMundo], Maybe Pieza)
parseSala lineas = (errores, pieza)
  where
    (campoId, campoDesc, salidas, objetos, errs) =
      foldl aplicar (NoVisto, NoVisto, [], [], []) lineas
    errores = reverse errs ++ faltas
    faltas =
      concat
        [ [anclar lineas "falta el campo SALA" | esNoVisto campoId]
        , [anclar lineas "falta el campo DESC" | esNoVisto campoDesc]
        ]
    pieza =
      case (campoId, campoDesc) of
        (Valido ident, Valido desc) | null errores ->
          Just
            ( PiezaSala
                ( Sala
                    ident
                    desc
                    (reverse salidas)
                    (reverse objetos)
                )
            )
        _ -> Nothing

    aplicar (i, d, ss, os, es) linea =
      case partirClave (textoLinea linea) of
        Nothing ->
          (i, d, ss, os, errorEn linea "la línea no tiene la forma CLAVE: valor" : es)
        Just ("SALA", valor) ->
          case i of
            NoVisto ->
              case reconocer identificador valor of
                Just ident -> (Valido ident, d, ss, os, es)
                Nothing -> (Invalido, d, ss, os, errorEn linea (identInvalido valor) : es)
            _ -> (i, d, ss, os, errorEn linea "campo SALA repetido" : es)
        Just ("DESC", valor) ->
          case d of
            NoVisto -> (i, Valido valor, ss, os, es)
            _ -> (i, d, ss, os, errorEn linea "campo DESC repetido" : es)
        Just ("SALIDA", valor) ->
          case reconocer salidaP valor of
            Just salida -> (i, d, salida : ss, os, es)
            Nothing -> (i, d, ss, os, errorEn linea (clasificarSalida valor) : es)
        Just ("OBJETO", valor) ->
          case reconocer identificador valor of
            Just ident -> (i, d, ss, ident : os, es)
            Nothing -> (i, d, ss, os, errorEn linea (identInvalido valor) : es)
        Just (clave, _)
          | claveConocida clave ->
              (i, d, ss, os, errorEn linea (noPermitida clave "un bloque de sala") : es)
          | otherwise ->
              (i, d, ss, os, errorEn linea (claveDesconocida clave) : es)

erroresSueltos :: [Linea] -> [ErrorMundo]
erroresSueltos = concatMap errorSuelto

errorSuelto :: Linea -> [ErrorMundo]
errorSuelto linea =
  case partirClave (textoLinea linea) of
    Nothing ->
      [errorEn linea "la línea no tiene la forma CLAVE: valor"]
    Just (clave, _)
      | claveConocida clave -> []
      | otherwise -> [errorEn linea (claveDesconocida clave)]

esNoVisto :: Campo a -> Bool
esNoVisto NoVisto = True
esNoVisto Invalido = False
esNoVisto (Valido _) = False

claveConocida :: String -> Bool
claveConocida clave =
  clave `elem` ["MUNDO", "INICIO", "META", "OBJETO", "DESC", "SALA", "SALIDA"]

claveDesconocida :: String -> String
claveDesconocida clave = "clave desconocida \"" ++ clave ++ "\""

noPermitida :: String -> String -> String
noPermitida clave lugar =
  "clave \"" ++ clave ++ "\" no permitida en " ++ lugar

identInvalido :: String -> String
identInvalido valor = "identificador inválido \"" ++ valor ++ "\""

metaP :: Reconocedor Meta
metaP = unoDe [llegarP, llevarP]
  where
    llegarP =
      cadena "llegar a " `luego` \_ ->
        identificador `luego` \sala ->
          exito (LlegarA sala)
    llevarP =
      cadena "llevar " `luego` \_ ->
        identificador `luego` \objeto ->
          cadena " a " `luego` \_ ->
            identificador `luego` \sala ->
              exito (LlevarA objeto sala)

salidaP :: Reconocedor Salida
salidaP =
  direccion `luego` \dir ->
    cadena " -> " `luego` \_ ->
      identificador `luego` \destino ->
        requisito `luego` \req ->
          exito (Salida dir destino req)
  where
    requisito =
      unoDe
        [ cadena " requiere " `luego` \_ ->
            identificador `luego` \objeto ->
              exito (Just objeto)
        , exito Nothing
        ]

clasificarMeta :: String -> String
clasificarMeta valor
  | Just resto <- stripPrefix "llegar a " valor =
      if ' ' `elem` resto
        then "META mal formada"
        else identInvalido resto
  | Just resto <- stripPrefix "llevar " valor =
      clasificarLlevar resto
  | otherwise = "META mal formada"

clasificarLlevar :: String -> String
clasificarLlevar resto =
  case words resto of
    [objeto, "a", sala]
      | reconocer identificador objeto == Nothing -> identInvalido objeto
      | reconocer identificador sala == Nothing -> identInvalido sala
      | otherwise -> "META mal formada"
    _ -> "META mal formada"

clasificarSalida :: String -> String
clasificarSalida valor =
  case words valor of
    (dir : resto)
      | reconocer direccion dir == Nothing && not ("->" `isInfixOf` dir) ->
          "dirección inexistente \"" ++ dir ++ "\""
      | Just _ <- reconocer direccion dir ->
          case resto of
            ["->", sala]
              | reconocer identificador sala == Nothing -> identInvalido sala
              | otherwise -> "SALIDA mal formada"
            ["->", sala, "requiere", objeto]
              | reconocer identificador sala == Nothing -> identInvalido sala
              | reconocer identificador objeto == Nothing -> identInvalido objeto
              | otherwise -> "SALIDA mal formada"
            _ -> "SALIDA mal formada"
      | otherwise -> "SALIDA mal formada"
    _ -> "SALIDA mal formada"
