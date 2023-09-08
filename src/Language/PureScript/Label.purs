module Language.PureScript.Label where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Language.PureScript.PSString (PSString, toPSString, fromPsString)
import Safe.Coerce (coerce)

-- |
-- Labels are used as record keys and row entry names. Labels newtype PSString
-- because records are indexable by PureScript strings at runtime.
--
newtype Label = Label PSString

derive instance Eq Label
derive instance Ord Label
derive instance Newtype Label _
derive instance Generic Label _
derive newtype instance Show Label
derive newtype instance Semigroup Label
derive newtype instance Monoid Label

fromLabel :: Label -> Json
fromLabel = unwrap >>> fromPsString

toLabel :: Json -> Either Json.DecodeError Label
toLabel = coerce <<< toPSString

