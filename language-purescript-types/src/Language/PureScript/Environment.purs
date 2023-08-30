module Language.PureScript.Environment where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (toString)
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

dataDeclTypeJSON :: DataDeclType -> JSON
dataDeclTypeJSON = JSON.fromString <<< showDataDeclType

jsonDataDeclType :: JSON -> Either String DataDeclType
jsonDataDeclType = toString >=> case _ of
  "data" -> pure Data
  "newtype" -> pure Newtype
  str -> Left $ "Expected 'data' or 'newtype' but got '" <> str <> "'."

kindType :: SourceType
kindType = srcTypeConstructor CPrim.tyType
