module Motor.Tipos
  ( Id (..)
  , Direccion (..)
  , Meta (..)
  , Objeto (..)
  , Salida (..)
  , Sala (..)
  , Mundo (..)
  , ErrorMundo (..)
  , Comando (..)
  , Fin (..)
  , Estado (..)
  , renderError
  ) where

import qualified Data.Map.Strict as Map

newtype Id = Id String
  deriving (Eq, Ord, Show)

data Direccion
  = Norte
  | Sur
  | Este
  | Oeste
  | Arriba
  | Abajo
  deriving (Eq, Ord, Show)

data Meta
  = LlegarA Id
  | LlevarA Id Id
  deriving (Eq, Show)

data Objeto = Objeto
  { objId :: Id
  , objDesc :: String
  }
  deriving (Eq, Show)

data Salida = Salida
  { salidaDir :: Direccion
  , salidaDestino :: Id
  , salidaRequiere :: Maybe Id
  }
  deriving (Eq, Show)

data Sala = Sala
  { salaId :: Id
  , salaDesc :: String
  , salaSalidas :: [Salida]
  , salaObjetos :: [Id]
  }
  deriving (Eq, Show)

data Mundo = Mundo
  { mundoTitulo :: String
  , mundoInicio :: Id
  , mundoMeta :: Meta
  , mundoObjetos :: Map.Map Id Objeto
  , mundoSalas :: Map.Map Id Sala
  }
  deriving (Eq, Show)

data ErrorMundo = ErrorMundo
  { errorLinea :: Maybe Int
  , errorMensaje :: String
  }
  deriving (Eq, Show)

-- Los textos de error no usan Show: el formato de una partida guardada
-- se decide aparte y no tiene que coincidir con esto.
renderError :: ErrorMundo -> String
renderError (ErrorMundo Nothing mensaje) =
  "Error: " ++ mensaje
renderError (ErrorMundo (Just linea) mensaje) =
  "Error: línea " ++ show linea ++ ": " ++ mensaje

data Comando
  = Ir Direccion
  | Mirar
  | Tomar Id
  | Soltar Id
  | Examinar Id
  | Inventario
  | Guardar FilePath
  | Cargar FilePath
  | Ayuda
  | Salir
  deriving (Eq, Show)

data Fin
  = Gano
  | Salio
  deriving (Eq, Show)

data Estado = Estado
  { estMundo :: Mundo
  , estSala :: Id
  , estInventario :: [Id]
  -- Objetos que están en una sala. Los del inventario no aparecen aquí.
  , estUbicacionObjetos :: Map.Map Id Id
  , estFin :: Maybe Fin
  }
  deriving (Eq, Show)
