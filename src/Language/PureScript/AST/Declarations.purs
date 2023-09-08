module Language.PureScript.AST.Declarations where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError(..), toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | What sort of declaration the kind signature applies to.
data KindSignatureFor
  = DataSig
  | NewtypeSig
  | TypeSynonymSig
  | ClassSig

derive instance Eq KindSignatureFor
derive instance Ord KindSignatureFor
derive instance Generic KindSignatureFor _
instance Show KindSignatureFor where
  show x = genericShow x

fromKindSignatureFor :: KindSignatureFor -> Json
fromKindSignatureFor = Json.fromString <<< case _ of
  DataSig -> "data"
  NewtypeSig -> "newtype"
  TypeSynonymSig -> "type"
  ClassSig -> "class"

toKindSignatureFor :: Json -> Either Json.DecodeError KindSignatureFor
toKindSignatureFor = toString >=> case _ of
  "data" -> pure DataSig
  "newtype" -> pure NewtypeSig
  "class" -> pure ClassSig
  "type" -> pure TypeSynonymSig
  str -> Left $ DecodeError $ "Expected 'data', 'newtype', 'class', or 'type' but got '" <> str <> "'"

