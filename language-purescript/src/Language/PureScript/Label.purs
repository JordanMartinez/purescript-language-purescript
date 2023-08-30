module Language.PureScript.Label where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import JSON (JSON)
import Language.PureScript.PSString (PSString, jsonPSString, psStringJSON)
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

labelJSON :: Label -> JSON
labelJSON = unwrap >>> psStringJSON

jsonLabel :: JSON -> Either String Label
jsonLabel = coerce <<< jsonPSString

