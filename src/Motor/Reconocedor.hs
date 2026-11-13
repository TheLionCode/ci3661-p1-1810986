module Motor.Reconocedor
  ( Reconocedor
  , falla
  , exito
  , satisface
  , cadena
  , token
  , unoDe
  , secuencia
  , luego
  , muchos
  , muchos1
  , reconocer
  , identificador
  , direccion
  ) where

import Motor.Tipos (Direccion (..), Id (..))

type Reconocedor a = ReadS a

falla :: Reconocedor a
falla _ = []

exito :: a -> Reconocedor a
exito valor entrada = [(valor, entrada)]

satisface :: (Char -> Bool) -> Reconocedor Char
satisface condicion entrada =
  case entrada of
    (c : resto) | condicion c -> [(c, resto)]
    _ -> []

cadena :: String -> Reconocedor String
cadena esperada = avanzar esperada
  where
    avanzar [] entrada = [(esperada, entrada)]
    avanzar (c : cs) (x : xs)
      | c == x = avanzar cs xs
    avanzar _ _ = []

token :: Reconocedor a -> Reconocedor a
token p = espacios `secuencia` p

unoDe :: [Reconocedor a] -> Reconocedor a
unoDe opciones entrada = concatMap (\opcion -> opcion entrada) opciones

secuencia :: Reconocedor a -> Reconocedor b -> Reconocedor b
secuencia p q = p `luego` const q

luego :: Reconocedor a -> (a -> Reconocedor b) -> Reconocedor b
luego p f entrada =
  concat [f valor resto | (valor, resto) <- p entrada]

muchos :: Reconocedor a -> Reconocedor [a]
muchos p = unoDe [muchos1 p, exito []]

muchos1 :: Reconocedor a -> Reconocedor [a]
muchos1 p =
  p `luego` \primero ->
    muchos p `luego` \resto ->
      exito (primero : resto)

-- Se queda con un resultado que consuma toda la entrada.
-- Si hay varios, usa el primero: los combinadores de abajo
-- prueban la alternativa más larga antes que la vacía.
reconocer :: Reconocedor a -> String -> Maybe a
reconocer p entrada =
  case [valor | (valor, "") <- p entrada] of
    (valor : _) -> Just valor
    [] -> Nothing

identificador :: Reconocedor Id
identificador =
  muchos1 (satisface esCaracterId) `luego` \cs ->
    exito (Id cs)

direccion :: Reconocedor Direccion
direccion =
  unoDe
    [ palabra "norte" Norte
    , palabra "sur" Sur
    , palabra "este" Este
    , palabra "oeste" Oeste
    , palabra "arriba" Arriba
    , palabra "abajo" Abajo
    ]

espacios :: Reconocedor ()
espacios =
  muchos (satisface esEspacio) `luego` \_ ->
    exito ()

palabra :: String -> a -> Reconocedor a
palabra texto valor =
  cadena texto `luego` \_ ->
    exito valor

esEspacio :: Char -> Bool
esEspacio c = c == ' ' || c == '\t'

esCaracterId :: Char -> Bool
esCaracterId c =
  (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c == '_'
    || c `elem` "áéíóúñü"
