module Language.PureScript.AST.Declarations.ChainId
  ( ChainId
  , mkChainId
  ) where

import Prelude

import Language.PureScript.AST.SourcePos as Pos

-- | For a given instance chain, stores the chain's file name and
-- | the starting source pos of the first instance in the chain.
-- | This data is used to determine which instances are part of
-- | the same instance chain.
newtype ChainId = ChainId { fileName :: String, startingSourcePos :: Pos.SourcePos }

derive newtype instance Eq ChainId
derive newtype instance Ord ChainId
derive newtype instance Show ChainId

mkChainId :: String -> Pos.SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId { fileName, startingSourcePos }
