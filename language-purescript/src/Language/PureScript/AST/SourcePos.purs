module Language.PureScript.AST.SourcePos where

import Prelude

import Codec.Json.Unidirectional.Value (fromArray, fromArray2, fromRecordN, fromRequired, toArray, toArray2, toInt, toRecordN, toRequired, toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Language.PureScript.Comments (Comment, commentJSON, jsonComment)

-- | Source annotation - position information and comments.
newtype SourceAnn = SourceAnn
  { span :: SourceSpan
  , comments :: Array Comment
  }

derive instance Eq SourceAnn
derive instance Newtype SourceAnn _
derive instance Generic SourceAnn _
instance Show SourceAnn where
  show x = genericShow x

sourceAnnJSON :: SourceAnn -> Json
sourceAnnJSON (SourceAnn { span, comments }) =
  fromArray2 (sourceSpanJSON span) (fromArray commentJSON comments)

jsonSourceAnn :: Json -> Either Json.DecodeError SourceAnn
jsonSourceAnn =
  toArray2 jsonSourceSpan (toArray jsonComment) \span comments ->
    SourceAnn { span, comments }

-- | Source position information
newtype SourcePos = SourcePos
  { line :: Int
  , column :: Int
  }

derive instance Eq SourcePos
derive instance Ord SourcePos
derive instance Newtype SourcePos _
derive instance Generic SourcePos _
instance Show SourcePos where
  show x = genericShow x

sourcePosJSON :: SourcePos -> Json
sourcePosJSON (SourcePos { line, column }) =
  fromArray2 (Json.fromInt line) (Json.fromInt column)

jsonSourcePos :: Json -> Either Json.DecodeError SourcePos
jsonSourcePos = toArray2 toInt toInt \line column ->
  SourcePos { line, column }

newtype SourceSpan = SourceSpan
  { name :: String
  , start :: SourcePos
  , end :: SourcePos
  }

derive instance Eq SourceSpan
derive instance Ord SourceSpan
derive instance Newtype SourceSpan _
derive instance Generic SourceSpan _
instance Show SourceSpan where
  show x = genericShow x

sourceSpanJSON :: SourceSpan -> Json
sourceSpanJSON = fromRecordN SourceSpan
  { name: fromRequired Json.fromString
  , start: fromRequired sourcePosJSON
  , end: fromRequired sourcePosJSON
  }

jsonSourceSpan :: Json -> Either Json.DecodeError SourceSpan
jsonSourceSpan = toRecordN SourceSpan
  { name: toRequired toString
  , start: toRequired jsonSourcePos
  , end: toRequired jsonSourcePos
  }

nullSourcePos :: SourcePos
nullSourcePos = SourcePos { line: 0, column: 0 }

nullSourceSpan :: SourceSpan
nullSourceSpan = SourceSpan { name: "", start: nullSourcePos, end: nullSourcePos }

nullSourceAnn :: SourceAnn
nullSourceAnn = SourceAnn { span: nullSourceSpan, comments: [] }
