module Language.PureScript.Docs.RenderedCode.Types where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Language.PureScript.Names (ConstructorName, Ident, ModuleName, ProperName, Qualified, TypeName, fromIdent, toIdent, toModuleName, toProperName, toQualified, fromModuleName, fromProperName, fromQualified)
import Safe.Coerce (coerce)

data ContainingModule
  = ThisModule
  | OtherModule ModuleName

derive instance Eq ContainingModule
derive instance Ord ContainingModule
derive instance Generic ContainingModule _
instance Show ContainingModule where
  show x = genericShow x

fromContainingModule :: ContainingModule -> Json
fromContainingModule = Json.fromJArray <<< case _ of
  ThisModule -> [ Json.fromString "ThisModule" ]
  OtherModule mn -> [ Json.fromString "OtherModule", fromModuleName mn ]

toContainingModule :: Json -> Either Json.DecodeError ContainingModule
toContainingModule = Json.altAccumulate current backwardsCompat
  where
  current = Json.toJArray >=> \ja -> do
    tag <- Json.underIndex 0 Json.toString ja
    case tag of
      "ThisModule" -> pure ThisModule
      "OtherModule" -> OtherModule <$> Json.underIndex 1 toModuleName ja
      str -> Left $ Json.DecodeError $ "Expected 'ThisModule' or 'OtherModule' but got '" <> str <> "'."

  -- For Json produced by compilers up to 0.10.5.
  backwardsCompat =
    Json.toNullNothingOrJust toModuleName
      >>> map (maybe ThisModule OtherModule)

data Link
  = NoLink
  | Link ContainingModule

derive instance Eq Link
derive instance Ord Link
derive instance Generic Link _
instance Show Link where
  show x = genericShow x

fromLink :: Link -> Json
fromLink = Json.fromJArray <<< case _ of
  NoLink -> [ Json.fromString "NoLink" ]
  Link cm -> [ Json.fromString "Link", fromContainingModule cm ]

toLink :: Json -> Either Json.DecodeError Link
toLink = Json.toJArray >=> \ja -> do
  tag <- Json.underIndex 0 Json.toString ja
  case tag of
    "NoLink" -> pure NoLink
    "Link" -> Json.underIndex 1 (toContainingModule >>> map Link) ja
    str -> Left $ Json.DecodeError $ "Expected 'NoLink' or 'Link' but got '" <> str <> "'."

data Namespace
  = ValueLevel
  | TypeLevel

derive instance Eq Namespace
derive instance Ord Namespace
derive instance Generic Namespace _
instance Show Namespace where
  show x = genericShow x

fromNamespace :: Namespace -> Json
fromNamespace = Json.fromString <<< case _ of
  ValueLevel -> "ValueLevel"
  TypeLevel -> "TypeLevel"

toNamespace :: Json -> Either Json.DecodeError Namespace
toNamespace = Json.toString >=> case _ of
  "ValueLevel" -> pure ValueLevel
  "TypeLevel" -> pure TypeLevel
  str -> Left $ Json.DecodeError $ "Expected 'ValueLevel' or 'TypeLevel' but got '" <> str <> "'."

-- |
-- A single element in a rendered code fragment. The intention is to support
-- multiple output formats. For example, plain text, or highlighted HTML.
--
data RenderedCodeElement
  = Syntax String
  | Keyword String
  | Space
  -- | Any symbol which you might or might not want to link to, in any
  -- namespace (value, type, or kind). Note that this is not related to the
  -- kind called Symbol for type-level strings.
  | Symbol Namespace String Link
  | Role String

derive instance Eq RenderedCodeElement
derive instance Ord RenderedCodeElement
derive instance Generic RenderedCodeElement _
instance Show RenderedCodeElement where
  show x = genericShow x

fromRenderedCodeElement :: RenderedCodeElement -> Json
fromRenderedCodeElement = Json.fromJArray <<< case _ of
  Syntax str -> [ Json.fromString "syntax", Json.fromString str ]
  Keyword str -> [ Json.fromString "keyword", Json.fromString str ]
  Space -> [ Json.fromString "space" ]
  Symbol ns str link -> [ Json.fromString "symbol", fromNamespace ns, Json.fromString str, fromLink link ]
  Role role -> [ Json.fromString "role", Json.fromString role ]

toRenderedCodeElement :: Json -> Either Json.DecodeError RenderedCodeElement
toRenderedCodeElement = Json.toJArray >=> \ja -> do
  ty <- Json.underIndex 0 Json.toString ja
  case ty of
    "syntax" ->
      Syntax <$> (Json.underIndex 1 Json.toString ja)
    "keyword" ->
      Keyword <$> (Json.underIndex 1 Json.toString ja)
    "space" ->
      pure Space
    "symbol" ->
      Symbol
        <$> (Json.underIndex 1 toNamespace ja)
        <*> (Json.underIndex 2 Json.toString ja)
        <*> (Json.underIndex 3 toLink ja)
    "role" ->
      Role <$> (Json.underIndex 1 Json.toString ja)
    str -> Left $ Json.DecodeError $ "Expected 'syntax', 'keyword', 'space', 'symbol' or 'role' but got " <> str <> "'."

newtype RenderedCode = RenderedCode (Array RenderedCodeElement)

derive instance Eq RenderedCode
derive instance Ord RenderedCode
derive instance Newtype RenderedCode _
derive instance Generic RenderedCode _
instance Show RenderedCode where
  show x = genericShow x

derive newtype instance Semigroup RenderedCode
derive newtype instance Monoid RenderedCode

fromRenderedCode :: RenderedCode -> Json
fromRenderedCode = unwrap >>> Json.fromArray fromRenderedCodeElement

toRenderedCode :: Json -> Either Json.DecodeError RenderedCode
toRenderedCode = coerce <<< Json.toArray toRenderedCodeElement

type FixityAlias = Qualified (Either (ProperName TypeName) (Either Ident (ProperName ConstructorName)))

fromFixityAlias :: FixityAlias -> Json
fromFixityAlias = fromQualified $ Json.fromEitherSingle fromProperName $ Json.fromEitherSingle fromIdent fromProperName

toFixityAlias :: Json -> Either Json.DecodeError FixityAlias
toFixityAlias = toQualified $ Json.toEitherSingle toProperName $ Json.toEitherSingle toIdent toProperName
