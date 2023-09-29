-- | Fresh variable supply
module Control.Monad.Supply where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.State (class MonadTrans, StateT(..), runStateT)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple (Tuple, fst)

newtype SupplyT m a = SupplyT (StateT Int m a)

derive newtype instance Functor m => Functor (SupplyT m)
derive instance Newtype (SupplyT m a) _
derive newtype instance Monad m => Apply (SupplyT m)
derive newtype instance Monad m => Applicative (SupplyT m)
derive newtype instance Monad m => Bind (SupplyT m)
derive newtype instance Monad m => Monad (SupplyT m)
derive newtype instance MonadTrans SupplyT
derive newtype instance MonadThrow e m => MonadThrow e (SupplyT m)
derive newtype instance MonadError e m => MonadError e (SupplyT m)
derive newtype instance MonadTell w m => MonadTell w (SupplyT m)
derive newtype instance MonadWriter w m => MonadWriter w (SupplyT m)
derive newtype instance MonadAsk r m => MonadAsk r (SupplyT m)
derive newtype instance MonadReader r m => MonadReader r (SupplyT m)
derive newtype instance (Monad m, Alternative m) => Alternative (SupplyT m)
derive newtype instance (Monad m, Alt m) => Alt (SupplyT m)
derive newtype instance (Monad m, Plus m) => Plus (SupplyT m)

-- derive instance Foldable (SupplyT m)
-- derive instance Traversable (SupplyT m)

unSupplyT :: forall m a. SupplyT m a -> StateT Int m a
unSupplyT = unwrap

runSupplyT :: forall m a. Int -> SupplyT m a -> m (Tuple a Int)
runSupplyT n = flip runStateT n <<< unSupplyT

evalSupplyT :: forall m a. Functor m => Int -> SupplyT m a -> m a
evalSupplyT n = map fst <<< runSupplyT n

type Supply = SupplyT Identity

runSupply :: forall a. Int -> Supply a -> Tuple a Int
runSupply n = un Identity <<< runSupplyT n
