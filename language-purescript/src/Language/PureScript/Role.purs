module Language.PureScript.Role where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError(..), toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- |
-- The role of a type constructor's parameter.
data Role
  = Nominal
  | Representational
  | Phantom

derive instance Eq Role
derive instance Ord Role
derive instance Generic Role _
instance Show Role where
  show x = genericShow x

fromRole :: Role -> Json
fromRole = Json.fromString <<< case _ of
  Representational -> "Representational"
  Nominal -> "Nominal"
  Phantom -> "Phantom"

toRole :: Json -> Either Json.DecodeError Role
toRole = toString >=> case _ of
  "Representational" -> pure Representational
  "Nominal" -> pure Nominal
  "Phantom" -> pure Phantom
  str -> Left $ (DecodeError $ "Expected 'Representational', 'Nominal', or 'Phantom', but got '" <> str <> "'.")
