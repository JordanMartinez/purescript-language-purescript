module Language.PureScript.TypeClassDictionaries where

import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple)
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Names (ClassName, Ident, ProperName, Qualified, disqualify, runProperName)
import Language.PureScript.Types (SourceType, SourceConstraint)

-- | Data representing a type class dictionary which is in scope
-- | - `tcdChain` - The instance chain
-- | - `tcdIndex` - Index of the instance chain
-- | - `tcdValue` - The value with which the dictionary can be accessed at runtime
-- | - `tcdPath` - How to obtain this instance via superclass relationships
-- | - `tcdClassName` - The name of the type class to which this type class instance applies
-- | - `tcdForAll` - Quantification of type variables in the instance head and dependencies
-- | - `tcdInstanceKinds` - The kinds to which this type class instance applies
-- | - `tcdInstanceTypes` - The types to which this type class instance applies
-- | - `tcdDependencies` - Type class dependencies which must be satisfied to construct this dictionary
-- | - `tcdDescription` - If this instance was unnamed, the type to use when describing it in error messages
--
newtype TypeClassDictionaryInScope v = TypeClassDictionaryInScope
  { tcdChain :: Maybe ChainId
  , tcdIndex :: Int
  , tcdValue :: v
  , tcdPath :: Array (Tuple (Qualified (ProperName ClassName)) Int)
  , tcdClassName :: Qualified (ProperName ClassName)
  , tcdForAll :: Array (Tuple String SourceType)
  , tcdInstanceKinds :: Array SourceType
  , tcdInstanceTypes :: Array SourceType
  , tcdDependencies :: Maybe (Array SourceConstraint)
  , tcdDescription :: Maybe SourceType
  }

derive instance Eq v => Eq (TypeClassDictionaryInScope v)
derive instance Ord v => Ord (TypeClassDictionaryInScope v)
derive instance Newtype (TypeClassDictionaryInScope v) _
derive instance Generic (TypeClassDictionaryInScope v) _
derive newtype instance Show v => Show (TypeClassDictionaryInScope v)

derive instance Functor TypeClassDictionaryInScope
derive instance Foldable TypeClassDictionaryInScope
derive instance Traversable TypeClassDictionaryInScope

type NamedDict = TypeClassDictionaryInScope (Qualified Ident)

-- | Generate a name for a superclass reference which can be used in
-- | generated code.
superclassName :: Qualified (ProperName ClassName) -> Int -> String
superclassName pn index = runProperName (disqualify pn) <> show index
