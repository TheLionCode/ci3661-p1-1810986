module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Motor.Tipos

estadoInicial :: Mundo -> Estado
estadoInicial mundo =
  let base =
        Estado
          { estMundo = mundo
          , estSala = mundoInicio mundo
          , estInventario = []
          , estUbicacionObjetos =
              Map.fromList
                [ (obj, salaId sala)
                | sala <- Map.elems (mundoSalas mundo)
                , obj <- salaObjetos sala
                ]
          , estFin = Nothing
          }
   in if metaCumplida base then base { estFin = Just Gano } else base

ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar _ est | Just _ <- estFin est = ([], est)
ejecutar (Guardar _) est = ([], est)
ejecutar (Cargar _) est = ([], est)
ejecutar comando est =
  case Map.lookup (estSala est) (mundoSalas (estMundo est)) of
    Nothing -> (["No estás en ninguna sala."], est)
    Just sala -> aplicar comando sala est

aplicar :: Comando -> Sala -> Estado -> ([String], Estado)
aplicar (Ir dir) sala est =
  case buscarSalida dir (salaSalidas sala) of
    Nothing ->
      (["No hay salida hacia " ++ fraseDir dir ++ "."], est)
    Just salida ->
      case salidaRequiere salida of
        Just necesario
          | necesario `notElem` estInventario est ->
              ( [ "La salida hacia "
                    ++ fraseDir dir
                    ++ " está bloqueada. Necesitas "
                    ++ textoId necesario
                    ++ "."
                ]
              , est
              )
        _ ->
          conMeta
            ["Vas hacia " ++ fraseDir dir ++ "."]
            est { estSala = salidaDestino salida }
aplicar Mirar sala est =
  ([salaDesc sala, mostrarObjetos est, mostrarSalidas sala], est)
aplicar (Tomar ident) _ est
  | Map.lookup ident (estUbicacionObjetos est) == Just (estSala est) =
      conMeta
        ["Tomas " ++ textoId ident ++ "."]
        est
          { estInventario = estInventario est ++ [ident]
          , estUbicacionObjetos = Map.delete ident (estUbicacionObjetos est)
          }
  | otherwise = (["No ves " ++ textoId ident ++ " aquí."], est)
aplicar (Soltar ident) _ est
  | ident `elem` estInventario est =
      conMeta
        ["Sueltas " ++ textoId ident ++ "."]
        est
          { estInventario = filter (/= ident) (estInventario est)
          , estUbicacionObjetos = Map.insert ident (estSala est) (estUbicacionObjetos est)
          }
  | otherwise = (["No llevas " ++ textoId ident ++ "."], est)
aplicar (Examinar ident) _ est
  | visible ident est =
      case Map.lookup ident (mundoObjetos (estMundo est)) of
        Just objeto -> ([objDesc objeto], est)
        Nothing -> (["No ves " ++ textoId ident ++ " por aquí."], est)
  | otherwise = (["No ves " ++ textoId ident ++ " por aquí."], est)
aplicar Inventario _ est =
  ( [ case estInventario est of
        [] -> "No llevas nada."
        objetos -> "Llevas: " ++ intercalate ", " (map textoId objetos) ++ "."
    ]
  , est
  )
aplicar Ayuda _ est =
  ( ["Comandos: ir <dirección>, mirar, tomar <objeto>, soltar <objeto>, examinar <objeto>, inventario, guardar <archivo>, cargar <archivo>, ayuda, salir. También vale la dirección sola, coger e inv."]
  , est
  )
aplicar Salir _ est =
  (["Hasta luego."], est { estFin = Just Salio })
aplicar (Guardar _) _ est = ([], est)
aplicar (Cargar _) _ est = ([], est)

conMeta :: [String] -> Estado -> ([String], Estado)
conMeta mensajes est
  | metaCumplida est =
      (mensajes ++ ["*** ¡Has ganado! ***"], est { estFin = Just Gano })
  | otherwise = (mensajes, est)

metaCumplida :: Estado -> Bool
metaCumplida est =
  case mundoMeta (estMundo est) of
    LlegarA sala -> estSala est == sala
    LlevarA objeto sala ->
      estSala est == sala && objeto `elem` estInventario est

visible :: Id -> Estado -> Bool
visible ident est =
  ident `elem` estInventario est
    || Map.lookup ident (estUbicacionObjetos est) == Just (estSala est)

buscarSalida :: Direccion -> [Salida] -> Maybe Salida
buscarSalida _ [] = Nothing
buscarSalida dir (salida : resto)
  | salidaDir salida == dir = Just salida
  | otherwise = buscarSalida dir resto

mostrarObjetos :: Estado -> String
mostrarObjetos est =
  case [textoId obj | (obj, sala) <- Map.toList (estUbicacionObjetos est), sala == estSala est] of
    [] -> "No hay objetos aquí."
    objetos -> "Objetos: " ++ intercalate ", " objetos ++ "."

mostrarSalidas :: Sala -> String
mostrarSalidas sala =
  case salaSalidas sala of
    [] -> "No hay salidas."
    salidas -> "Salidas: " ++ intercalate ", " (map describir salidas) ++ "."
  where
    describir salida =
      fraseDir (salidaDir salida)
        ++ case salidaRequiere salida of
          Nothing -> ""
          Just objeto -> " (requiere " ++ textoId objeto ++ ")"

textoId :: Id -> String
textoId (Id texto) = texto

fraseDir :: Direccion -> String
fraseDir Norte = "el norte"
fraseDir Sur = "el sur"
fraseDir Este = "el este"
fraseDir Oeste = "el oeste"
fraseDir Arriba = "arriba"
fraseDir Abajo = "abajo"
