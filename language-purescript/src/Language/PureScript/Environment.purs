module Language.PureScript.Environment where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Language.PureScript.Constants.Prim as CPrim
import Language.PureScript.Types (SourceType, srcTypeConstructor)

data DataDeclType
  = Data
  | Newtype

derive instance Eq DataDeclType
derive instance Ord DataDeclType
derive instance Generic DataDeclType _
instance Show DataDeclType where
  show x = showDataDeclType x

showDataDeclType :: DataDeclType -> String
showDataDeclType Data = "data"
showDataDeclType Newtype = "newtype"

dataDeclTypeJSON :: DataDeclType -> Json
dataDeclTypeJSON = Json.fromString <<< showDataDeclType

jsonDataDeclType :: Json -> Either Json.DecodeError DataDeclType
jsonDataDeclType = Json.toString >=> case _ of
  "data" -> pure Data
  "newtype" -> pure Newtype
  str -> Left $ Json.DecodeError $ "Expected 'data' or 'newtype' but got '" <> str <> "'."

kindType :: SourceType
kindType = srcTypeConstructor CPrim.tyType
