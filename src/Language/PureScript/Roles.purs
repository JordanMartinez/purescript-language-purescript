module Language.PureScript.Roles where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError(..), toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | The role of a type constructor's parameter.
data Role
  -- | This parameter's identity affects the representation of the type it is parameterising.
  = Nominal
  -- | This parameter's representation affects the representation of the type it is parameterising.
  | Representational
  -- | This parameter has no effect on the representation of the type it is parameterising.
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

displayRole :: Role -> String
displayRole r = case r of
  Nominal -> "nominal"
  Representational -> "representational"
  Phantom -> "phantom"
