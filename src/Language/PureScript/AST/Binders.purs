-- | Case binders
module Language.PureScript.AST.Binders where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), snd)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.Names (ConstructorName, Ident, OpName, ProperName, Qualified, ValueOpName)
import Language.PureScript.Types (SourceType)

-- | Data type for binders
data Binder
  -- | Wildcard binder
  = NullBinder
  -- | A binder which matches a literal
  | LiteralBinder SourceSpan (Literal Binder)
  -- | A binder which binds an identifier
  | VarBinder SourceSpan Ident
  -- | A binder which matches a data constructor
  | ConstructorBinder SourceSpan (Qualified (ProperName ConstructorName)) (Array Binder)
  -- | A operator alias binder. During the rebracketing phase of desugaring,
  -- | this data constructor will be removed.
  | OpBinder SourceSpan (Qualified (OpName ValueOpName))
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- | this data constructor will be removed.
  | BinaryNoParensBinder Binder Binder Binder
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- | data constructor will be removed.
  -- |
  -- | Note: although it seems this constructor is not used, it _is_ useful,
  -- | since it prevents certain traversals from matching.
  | ParensInBinder Binder
  -- | A binder which binds its input to an identifier
  | NamedBinder SourceSpan Ident Binder
  -- | A binder with source position information
  | PositionedBinder SourceSpan (Array Comment) Binder
  -- | A binder with a type annotation
  | TypedBinder SourceType Binder

derive instance Generic Binder _
instance Show Binder where
  show x = genericShow x

-- | Manual Eq and Ord instances for `Binder` were added on 2018-03-05. Comparing
-- | the `SourceSpan` values embedded in some of the data constructors of `Binder`
-- | was expensive. This made exhaustiveness checking observably slow for code
-- | such as the `explode` function in `test/purs/passing/LargeSumTypes.purs`.
-- | Custom instances were written to skip comparing the `SourceSpan` values. Only
-- | the `Ord` instance was needed for the speed-up, but I did not want the `Eq`
-- | to have mismatched behavior.
instance Eq Binder where
  eq = case _, _ of
    NullBinder, NullBinder ->
      true
    (LiteralBinder _ lb), (LiteralBinder _ lb') ->
      lb == lb'
    (VarBinder _ ident), (VarBinder _ ident') ->
      ident == ident'
    (ConstructorBinder _ qpc bs), (ConstructorBinder _ qpc' bs') ->
      qpc == qpc' && bs == bs'
    (OpBinder _ qov), (OpBinder _ qov') ->
      qov == qov'
    (BinaryNoParensBinder b1 b2 b3), (BinaryNoParensBinder b1' b2' b3') ->
      b1 == b1' && b2 == b2' && b3 == b3'
    (ParensInBinder b), (ParensInBinder b') ->
      b == b'
    (NamedBinder _ ident b), (NamedBinder _ ident' b') ->
      ident == ident' && b == b'
    (PositionedBinder _ comments b), (PositionedBinder _ comments' b') ->
      comments == comments' && b == b'
    (TypedBinder ty b), (TypedBinder ty' b') ->
      ty == ty' && b == b'
    _, _ ->
      false

instance Ord Binder where
  compare = case _, _ of
    NullBinder, NullBinder ->
      EQ
    LiteralBinder _ lb, LiteralBinder _ lb' ->
      compare lb lb'
    VarBinder _ ident, VarBinder _ ident' ->
      compare ident ident'
    ConstructorBinder _ qpc bs, ConstructorBinder _ qpc' bs' ->
      compare qpc qpc' <> compare bs bs'
    OpBinder _ qov, OpBinder _ qov' ->
      compare qov qov'
    BinaryNoParensBinder b1 b2 b3, BinaryNoParensBinder b1' b2' b3' ->
      compare b1 b1' <> compare b2 b2' <> compare b3 b3'
    ParensInBinder b, ParensInBinder b' ->
      compare b b'
    NamedBinder _ ident b, NamedBinder _ ident' b' ->
      compare ident ident' <> compare b b'
    PositionedBinder _ comments b, PositionedBinder _ comments' b' ->
      compare comments comments' <> compare b b'
    TypedBinder ty b, TypedBinder ty' b' ->
      compare ty ty' <> compare b b'
    binder, binder' ->
      compare (orderOf binder) (orderOf binder')
      where
      orderOf :: Binder -> Int
      orderOf NullBinder = 0
      orderOf (LiteralBinder _ _) = 1
      orderOf (VarBinder _ _) = 2
      orderOf (ConstructorBinder _ _ _) = 3
      orderOf (OpBinder _ _) = 4
      orderOf (BinaryNoParensBinder _ _ _) = 5
      orderOf (ParensInBinder _) = 6
      orderOf (NamedBinder _ _ _) = 7
      orderOf (PositionedBinder _ _ _) = 8
      orderOf (TypedBinder _ _) = 9

-- | Collect all names introduced in binders in an expression
binderNames :: Binder -> Array Ident
binderNames = map snd <<< binderNamesWithSpans

binderNamesWithSpans :: Binder -> Array (Tuple SourceSpan Ident)
binderNamesWithSpans binder = ST.run do
  arr <- STA.new
  go arr binder
  STA.unsafeFreeze arr
  where
  go :: forall h. STArray h (Tuple SourceSpan Ident) -> Binder -> ST h Unit
  go ns = case _ of
    (LiteralBinder _ b) ->
      lit ns b
    (VarBinder ss name) ->
      void $ STA.push (Tuple ss name) ns
    (ConstructorBinder _ _ bs) -> do
      ST.foreach bs \next ->
        go ns next
    (BinaryNoParensBinder b1 b2 b3) -> do
      ST.foreach [ b1, b2, b3 ] \next ->
        go ns next
    (ParensInBinder b) ->
      go ns b
    (NamedBinder ss name b) -> do
      void $ STA.push (Tuple ss name) ns
      go ns b
    (PositionedBinder _ _ b) ->
      go ns b
    (TypedBinder _ b) ->
      go ns b
    _ ->
      pure unit

  lit :: forall h. STArray h (Tuple SourceSpan Ident) -> Literal Binder -> ST h Unit
  lit ns = case _ of
    (ObjectLiteral bs) -> do
      ST.foreach bs \next ->
        go ns $ snd next
    (ArrayLiteral bs) -> do
      ST.foreach bs \next ->
        go ns next
    _ ->
      pure unit

isIrrefutable :: Binder -> Boolean
isIrrefutable = case _ of
  NullBinder -> true
  (VarBinder _ _) -> true
  (PositionedBinder _ _ b) -> isIrrefutable b
  (TypedBinder _ b) -> isIrrefutable b
  _ -> false
