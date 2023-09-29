-- | A class for monads supporting a supply of fresh names
module Control.Monad.Supply.Class where

import Prelude

import Control.Monad.RWS (RWST, lift)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Supply (SupplyT(..))
import Control.Monad.Writer (WriterT)

class Monad m <= MonadSupply m where
  fresh :: m Int
  peek :: m Int

instance Monad m => MonadSupply (SupplyT m) where
  fresh = SupplyT do
    n <- get
    put (n + 1)
    pure n
  peek = SupplyT get

instance MonadSupply m => MonadSupply (StateT s m) where
  fresh = lift fresh
  peek = lift peek

instance (Monoid w, MonadSupply m) => MonadSupply (WriterT w m) where
  fresh = lift fresh
  peek = lift peek

instance (Monoid w, MonadSupply m) => MonadSupply (RWST r w s m) where
  fresh = lift fresh
  peek = lift peek

freshName :: forall m. MonadSupply m => m String
freshName = (append "$" <<< show) <$> fresh
