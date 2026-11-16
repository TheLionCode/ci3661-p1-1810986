module Motor.Mundo
  ( leerMundo
  ) where

import Data.List (isInfixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

data Cabecera = Cabecera
  { cabTitulo :: String
  , cabInicio :: (Int, Id)
  , cabMeta :: (Int, Meta)
  , cabTurnos :: Maybe Int
  }

data Pieza
  = PiezaObjeto Int Objeto
  | PiezaSala Int Id String [(Int, Salida)] [(Int, Id)]

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
              let piezas = [pieza | (_, Just pieza) <- bloques]
                  semantica = validar cab piezas
               in if null semantica
                    then Right (construir cab piezas)
                    else Left semantica
            (_, []) ->
              Left [errorSinLinea "no se pudo leer la cabecera"]
            (_, errs) ->
              Left errs

construir :: Cabecera -> [Pieza] -> Mundo
construir cab piezas =
  Mundo
    { mundoTitulo = cabTitulo cab
    , mundoInicio = snd (cabInicio cab)
    , mundoMeta = snd (cabMeta cab)
    , mundoObjetos =
        Map.fromList
          [(objId objeto, objeto) | PiezaObjeto _ objeto <- piezas]
    , mundoSalas =
        Map.fromList
          [ (sid, Sala sid desc (map snd salidas) (map snd objetos))
          | PiezaSala _ sid desc salidas objetos <- piezas
          ]
    , mundoTurnos = cabTurnos cab
    }

validar :: Cabecera -> [Pieza] -> [ErrorMundo]
validar cab piezas =
  duplicados "objeto" [(linea, objId objeto) | PiezaObjeto linea objeto <- piezas]
    ++ duplicados "sala" [(linea, sid) | PiezaSala linea sid _ _ _ <- piezas]
    ++ erroresMeta
    ++ concatMap erroresSala piezas
    ++ erroresRepetidos
    ++ erroresAlcance
    ++ erroresFaltantes
  where
    objetosDef = Map.fromList [(objId objeto, linea) | PiezaObjeto linea objeto <- piezas]
    salasDef = Map.fromList [(sid, linea) | PiezaSala linea sid _ _ _ <- piezas]
    colocados =
      foldl
        anotar
        Map.empty
        [ (obj, linea)
        | PiezaSala _ _ _ _ objetos <- piezas
        , (linea, obj) <- objetos
        ]
    anotar mapa (obj, linea) =
      Map.insertWith (\nuevo viejo -> viejo ++ nuevo) obj [linea] mapa

    erroresMeta =
      let (lineaInicio, inicio) = cabInicio cab
          (lineaMeta, meta) = cabMeta cab
          faltaSala linea sala =
            [ errLinea linea ("la sala \"" ++ textoId sala ++ "\" no existe.")
            | not (Map.member sala salasDef)
            ]
          faltaObjeto linea objeto =
            [ errLinea linea ("el objeto \"" ++ textoId objeto ++ "\" no existe.")
            | not (Map.member objeto objetosDef)
            ]
       in case meta of
            LlegarA sala ->
              faltaSala lineaInicio inicio ++ faltaSala lineaMeta sala
            LlevarA objeto sala ->
              faltaSala lineaInicio inicio
                ++ faltaObjeto lineaMeta objeto
                ++ faltaSala lineaMeta sala

    erroresSala (PiezaObjeto _ _) = []
    erroresSala (PiezaSala _ sid _ salidas objetos) =
      revisarSalidas sid salidas Set.empty
        ++ concatMap (revisarObjeto sid) objetos

    revisarSalidas _ [] _ = []
    revisarSalidas sid ((linea, salida) : resto) vistas =
      let dir = salidaDir salida
          destino = salidaDestino salida
          repetida =
            [ errLinea linea
                ( "la sala \""
                    ++ textoId sid
                    ++ "\" tiene dos salidas hacia "
                    ++ fraseDir dir
                    ++ "."
                )
            | Set.member dir vistas
            ]
          inexistente =
            [ errLinea linea
                ( "la salida de \""
                    ++ textoId sid
                    ++ "\" lleva a \""
                    ++ textoId destino
                    ++ "\", que no existe."
                )
            | not (Map.member destino salasDef)
            ]
          requisito =
            case salidaRequiere salida of
              Just objeto
                | not (Map.member objeto objetosDef) ->
                    [ errLinea linea
                        ( "la salida de \""
                            ++ textoId sid
                            ++ "\" pide \""
                            ++ textoId objeto
                            ++ "\", que no existe."
                        )
                    ]
              _ -> []
       in repetida
            ++ inexistente
            ++ requisito
            ++ revisarSalidas sid resto (Set.insert dir vistas)

    revisarObjeto sid (linea, objeto)
      | Map.member objeto objetosDef = []
      | otherwise =
          [ errLinea linea
              ( "la sala \""
                  ++ textoId sid
                  ++ "\" contiene \""
                  ++ textoId objeto
                  ++ "\", que no está definido."
              )
          ]

    erroresRepetidos =
      [ errLinea linea
          ( "el objeto \""
              ++ textoId objeto
              ++ "\" está en más de una sala."
          )
      | (objeto, lineas) <- Map.toList colocados
      , linea <- tomarSegunda lineas
      , Map.member objeto objetosDef
      ]

    erroresAlcance
      | not (Map.member inicio salasDef) = []
      | otherwise =
          [ errLinea linea
              ( "la sala \""
                  ++ textoId sid
                  ++ "\" no se alcanza desde el inicio."
              )
          | (sid, linea) <- Map.toList salasDef
          , not (Set.member sid visitadas)
          ]
      where
        (_, inicio) = cabInicio cab
        visitadas = alcanzables grafo inicio
        grafo =
          Map.fromListWith
            (++)
            [ (sid, [destino])
            | PiezaSala _ sid _ salidas _ <- piezas
            , (_, salida) <- salidas
            , let destino = salidaDestino salida
            , Map.member destino salasDef
            ]

    erroresFaltantes = snd (foldl anotarFalta (Set.empty, []) necesidades)
      where
        necesidadesMeta =
          case cabMeta cab of
            (_, LlegarA _) -> []
            (linea, LlevarA objeto _) -> [(linea, objeto)]
        necesidades =
          necesidadesMeta
            ++ [ (linea, objeto)
               | PiezaSala _ _ _ salidas _ <- piezas
               , (linea, salida) <- salidas
               , Just objeto <- [salidaRequiere salida]
               ]
        anotarFalta (vistos, errs) (linea, objeto)
          | Set.member objeto vistos = (vistos, errs)
          | not (Map.member objeto objetosDef) = (vistos, errs)
          | colocado objeto = (Set.insert objeto vistos, errs)
          | otherwise =
              ( Set.insert objeto vistos
              , errs
                  ++ [ errLinea linea
                         ( "hace falta \""
                             ++ textoId objeto
                             ++ "\" y no está en ninguna sala."
                         )
                     ]
              )
        colocado objeto =
          maybe False (not . null) (Map.lookup objeto colocados)

alcanzables :: Map.Map Id [Id] -> Id -> Set.Set Id
alcanzables grafo origen = recorrer Set.empty [origen]
  where
    recorrer vistos [] = vistos
    recorrer vistos (actual : cola)
      | Set.member actual vistos = recorrer vistos cola
      | otherwise =
          recorrer
            (Set.insert actual vistos)
            (cola ++ Map.findWithDefault [] actual grafo)

duplicados :: String -> [(Int, Id)] -> [ErrorMundo]
duplicados clase pares = snd (foldl paso (Set.empty, []) pares)
  where
    paso (vistos, errs) (linea, ident)
      | Set.member ident vistos =
          ( vistos
          , errs
              ++ [ errLinea linea
                     ( "identificador de "
                         ++ clase
                         ++ " repetido \""
                         ++ textoId ident
                         ++ "\"."
                     )
                 ]
          )
      | otherwise = (Set.insert ident vistos, errs)

tomarSegunda :: [a] -> [a]
tomarSegunda (_ : segunda : _) = [segunda]
tomarSegunda _ = []

errLinea :: Int -> String -> ErrorMundo
errLinea linea mensaje = ErrorMundo (Just linea) mensaje

textoId :: Id -> String
textoId (Id texto) = texto

fraseDir :: Direccion -> String
fraseDir Norte = "el norte"
fraseDir Sur = "el sur"
fraseDir Este = "el este"
fraseDir Oeste = "el oeste"
fraseDir Arriba = "arriba"
fraseDir Abajo = "abajo"

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

parseCabecera :: [Linea] -> ([ErrorMundo], Maybe Cabecera)
parseCabecera lineas = (errores, resultado)
  where
    (mundo, inicio, meta, turnos, errs) =
      foldl aplicar (NoVisto, NoVisto, NoVisto, NoVisto, []) lineas
    errores = reverse errs ++ faltas
    faltas =
      concat
        [ [anclar lineas "falta el campo MUNDO" | esNoVisto mundo]
        , [anclar lineas "falta el campo INICIO" | esNoVisto inicio]
        , [anclar lineas "falta el campo META" | esNoVisto meta]
        ]
    resultado =
      case (mundo, inicio, meta, leerTurnos turnos) of
        (Valido titulo, Valido inicioValor, Valido metaValor, Just limite)
          | null errores -> Just (Cabecera titulo inicioValor metaValor limite)
        _ -> Nothing

    aplicar (m, i, mt, t, es) linea =
      case partirClave (textoLinea linea) of
        Nothing ->
          (m, i, mt, t, errorEn linea "la línea no tiene la forma CLAVE: valor" : es)
        Just ("MUNDO", valor) ->
          case m of
            NoVisto -> (Valido valor, i, mt, t, es)
            _ -> (m, i, mt, t, errorEn linea "campo MUNDO repetido" : es)
        Just ("INICIO", valor) ->
          case i of
            NoVisto ->
              case reconocer identificador valor of
                Just ident -> (m, Valido (numeroLinea linea, ident), mt, t, es)
                Nothing ->
                  (m, Invalido, mt, t, errorEn linea (identInvalido valor) : es)
            _ -> (m, i, mt, t, errorEn linea "campo INICIO repetido" : es)
        Just ("META", valor) ->
          case mt of
            NoVisto ->
              case reconocer metaP valor of
                Just metaValor -> (m, i, Valido (numeroLinea linea, metaValor), t, es)
                Nothing ->
                  (m, i, Invalido, t, errorEn linea (clasificarMeta valor) : es)
            _ -> (m, i, mt, t, errorEn linea "campo META repetido" : es)
        Just ("TURNOS", valor) ->
          case t of
            NoVisto ->
              case enteroPositivo valor of
                Just n -> (m, i, mt, Valido n, es)
                Nothing ->
                  (m, i, mt, Invalido, errorEn linea "TURNOS debe ser un entero positivo." : es)
            _ -> (m, i, mt, t, errorEn linea "campo TURNOS repetido" : es)
        Just (clave, _)
          | claveConocida clave ->
              (m, i, mt, t, errorEn linea (noPermitida clave "la cabecera") : es)
          | otherwise ->
              (m, i, mt, t, errorEn linea (claveDesconocida clave) : es)

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
        (Valido (lineaId, ident), Valido desc) | null errores ->
          Just (PiezaObjeto lineaId (Objeto ident desc))
        _ -> Nothing

    aplicar (i, d, es) linea =
      case partirClave (textoLinea linea) of
        Nothing ->
          (i, d, errorEn linea "la línea no tiene la forma CLAVE: valor" : es)
        Just ("OBJETO", valor) ->
          case i of
            NoVisto ->
              case reconocer identificador valor of
                Just ident -> (Valido (numeroLinea linea, ident), d, es)
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
        (Valido (lineaId, ident), Valido desc) | null errores ->
          Just (PiezaSala lineaId ident desc (reverse salidas) (reverse objetos))
        _ -> Nothing

    aplicar (i, d, ss, os, es) linea =
      case partirClave (textoLinea linea) of
        Nothing ->
          (i, d, ss, os, errorEn linea "la línea no tiene la forma CLAVE: valor" : es)
        Just ("SALA", valor) ->
          case i of
            NoVisto ->
              case reconocer identificador valor of
                Just ident -> (Valido (numeroLinea linea, ident), d, ss, os, es)
                Nothing -> (Invalido, d, ss, os, errorEn linea (identInvalido valor) : es)
            _ -> (i, d, ss, os, errorEn linea "campo SALA repetido" : es)
        Just ("DESC", valor) ->
          case d of
            NoVisto -> (i, Valido valor, ss, os, es)
            _ -> (i, d, ss, os, errorEn linea "campo DESC repetido" : es)
        Just ("SALIDA", valor) ->
          case reconocer salidaP valor of
            Just salida -> (i, d, (numeroLinea linea, salida) : ss, os, es)
            Nothing -> (i, d, ss, os, errorEn linea (clasificarSalida valor) : es)
        Just ("OBJETO", valor) ->
          case reconocer identificador valor of
            Just ident -> (i, d, ss, (numeroLinea linea, ident) : os, es)
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
  clave `elem` ["MUNDO", "INICIO", "META", "TURNOS", "OBJETO", "DESC", "SALA", "SALIDA"]

-- NoVisto es un mundo sin límite. Invalido ya dejó un error y no produce valor.
leerTurnos :: Campo Int -> Maybe (Maybe Int)
leerTurnos NoVisto = Just Nothing
leerTurnos Invalido = Nothing
leerTurnos (Valido n) = Just (Just n)

enteroPositivo :: String -> Maybe Int
enteroPositivo texto
  | null texto = Nothing
  | all esDigito texto =
      let valor = foldl (\acc c -> acc * 10 + fromEnum c - fromEnum '0') 0 texto
       in if valor > 0 then Just valor else Nothing
  | otherwise = Nothing

esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'

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
