module Language.PureScript.Docs.RenderedCode.Types where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (eitherJSON, fromArray, jsonEither, toArray, toJArray, toNullNothingOrJust, toString, underIndex)
import Language.PureScript.Names (ConstructorName, Ident, ModuleName, ProperName, Qualified, TypeName, identJSON, jsonIdent, jsonModuleName, jsonProperName, jsonQualified, moduleNameJSON, properNameJSON, qualifiedJSON)
import Safe.Coerce (coerce)

data ContainingModule
  = ThisModule
  | OtherModule ModuleName

derive instance Eq ContainingModule
derive instance Ord ContainingModule
derive instance Generic ContainingModule _
instance Show ContainingModule where
  show x = genericShow x

containingModuleJSON :: ContainingModule -> JSON
containingModuleJSON = JSON.fromArray <<< case _ of
  ThisModule -> [ JSON.fromString "ThisModule" ]
  OtherModule mn -> [ JSON.fromString "OtherModule", moduleNameJSON mn ]

jsonContainingModule :: JSON -> Either String ContainingModule
jsonContainingModule j' = current j' <|> backwardsCompat j'
  where
  current j = do
    ja <- toJArray j
    underIndex 0 ja toString >>= case _ of
      "ThisModule" -> pure ThisModule
      "OtherModule" -> OtherModule <$> do
        underIndex 1 ja jsonModuleName
      str -> Left $ "Expected 'ThisModule' or 'OtherModule' but got '" <> str <> "'."

  -- For JSON produced by compilers up to 0.10.5.
  backwardsCompat =
    toNullNothingOrJust jsonModuleName
      >>> map (maybe ThisModule OtherModule)

data Link
  = NoLink
  | Link ContainingModule

derive instance Eq Link
derive instance Ord Link
derive instance Generic Link _
instance Show Link where
  show x = genericShow x

linkJSON :: Link -> JSON
linkJSON = JSON.fromArray <<< case _ of
  NoLink -> [ JSON.fromString "NoLink" ]
  Link cm -> [ JSON.fromString "Link", containingModuleJSON cm ]

jsonLink :: JSON -> Either String Link
jsonLink j = do
  ja <- toJArray j
  underIndex 0 ja toString >>= case _ of
    "NoLink" -> pure NoLink
    "Link" -> underIndex 1 ja (jsonContainingModule >>> map Link)
    str -> Left $ "Expected 'NoLink' or 'Link' but got '" <> str <> "'."

data Namespace
  = ValueLevel
  | TypeLevel

derive instance Eq Namespace
derive instance Ord Namespace
derive instance Generic Namespace _
instance Show Namespace where
  show x = genericShow x

namespaceJSON :: Namespace -> JSON
namespaceJSON = JSON.fromString <<< case _ of
  ValueLevel -> "ValueLevel"
  TypeLevel -> "TypeLevel"

jsonNamespace :: JSON -> Either String Namespace
jsonNamespace = toString >=> case _ of
  "ValueLevel" -> pure ValueLevel
  "TypeLevel" -> pure TypeLevel
  str -> Left $ "Expected 'ValueLevel' or 'TypeLevel' but got '" <> str <> "'."

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

renderedCodeElementJSON :: RenderedCodeElement -> JSON
renderedCodeElementJSON = JSON.fromArray <<< case _ of
  Syntax str -> [ JSON.fromString "syntax", JSON.fromString str ]
  Keyword str -> [ JSON.fromString "keyword", JSON.fromString str ]
  Space -> [ JSON.fromString "space" ]
  Symbol ns str link -> [ JSON.fromString "symbol", namespaceJSON ns, JSON.fromString str, linkJSON link ]
  Role role -> [ JSON.fromString "role", JSON.fromString role ]

jsonRenderedCodeElement :: JSON -> Either String RenderedCodeElement
jsonRenderedCodeElement = toJArray >=> \ja -> do
  ty <- underIndex 0 ja toString
  case ty of
    "syntax" ->
      Syntax <$> (underIndex 1 ja toString)
    "keyword" ->
      Keyword <$> (underIndex 1 ja toString)
    "space" ->
      pure Space
    "symbol" ->
      Symbol
        <$> (underIndex 1 ja jsonNamespace)
        <*> (underIndex 2 ja toString)
        <*> (underIndex 3 ja jsonLink)
    "role" ->
      Role <$> (underIndex 1 ja toString)
    str -> Left $ "Expected 'syntax', 'keyword', 'space', 'symbol' or 'role' but got " <> str <> "'."

newtype RenderedCode = RenderedCode (Array RenderedCodeElement)

derive instance Eq RenderedCode
derive instance Ord RenderedCode
derive instance Newtype RenderedCode _
derive instance Generic RenderedCode _
instance Show RenderedCode where
  show x = genericShow x

derive newtype instance Semigroup RenderedCode
derive newtype instance Monoid RenderedCode

renderedCodeJSON :: RenderedCode -> JSON
renderedCodeJSON = unwrap >>> fromArray renderedCodeElementJSON

jsonRenderedCode :: JSON -> Either String RenderedCode
jsonRenderedCode = coerce <<< toArray jsonRenderedCodeElement

type FixityAlias = Qualified (Either (ProperName TypeName) (Either Ident (ProperName ConstructorName)))

fixityAliasJSON :: FixityAlias -> JSON
fixityAliasJSON = qualifiedJSON $ eitherJSON properNameJSON $ eitherJSON identJSON properNameJSON

jsonFixityAlias :: JSON -> Either String FixityAlias
jsonFixityAlias = jsonQualified $ jsonEither jsonProperName $ jsonEither jsonIdent jsonProperName
