module Motor.Comandos
  ( leerComando
  ) where

import Motor.Reconocedor
  ( Reconocedor
  , cadena
  , direccion
  , exito
  , identificador
  , luego
  , reconocer
  , secuencia
  , token
  , unoDe
  )
import Motor.Tipos

leerComando :: String -> Maybe Comando
leerComando entrada =
  case trim entrada of
    "" -> Nothing
    limpia -> reconocer comandoP limpia

comandoP :: Reconocedor Comando
comandoP =
  unoDe
    [ verbo "ir" `secuencia` token direccion `luego` \dir -> exito (Ir dir)
    , token direccion `luego` \dir -> exito (Ir dir)
    , verbo "mirar" `luego` \_ -> exito Mirar
    , conObjeto (unoDe [cadena "tomar", cadena "coger"]) Tomar
    , conObjeto (cadena "soltar") Soltar
    , conObjeto (cadena "examinar") Examinar
    , verbo "inventario" `luego` \_ -> exito Inventario
    , verbo "inv" `luego` \_ -> exito Inventario
    , verbo "guardar" `secuencia` token resto `luego` \ruta -> exito (Guardar ruta)
    , verbo "cargar" `secuencia` token resto `luego` \ruta -> exito (Cargar ruta)
    , verbo "ayuda" `luego` \_ -> exito Ayuda
    , verbo "salir" `luego` \_ -> exito Salir
    ]

conObjeto :: Reconocedor String -> (Id -> Comando) -> Reconocedor Comando
conObjeto nombre construir =
  nombre `luego` \_ ->
    borde `luego` \_ ->
      token identificador `luego` \ident ->
        exito (construir ident)

verbo :: String -> Reconocedor ()
verbo nombre =
  cadena nombre `luego` \_ ->
    borde

-- Después del verbo tiene que terminar la línea o venir un espacio.
-- Así "guardarfoo" no cuenta como guardar, e "inv" no se come "inventario".
borde :: Reconocedor ()
borde entrada =
  case entrada of
    [] -> [((), entrada)]
    (c : _)
      | esEspacio c -> [((), entrada)]
      | otherwise -> []

resto :: Reconocedor String
resto entrada
  | null entrada = []
  | otherwise = [(entrada, "")]

trim :: String -> String
trim = quitarFin . quitarInicio
  where
    quitarInicio = dropWhile esEspacio
    quitarFin = reverse . dropWhile esEspacio . reverse

esEspacio :: Char -> Bool
esEspacio c = c == ' ' || c == '\t' || c == '\r'
