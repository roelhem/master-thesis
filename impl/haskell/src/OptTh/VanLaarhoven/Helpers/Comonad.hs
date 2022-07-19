module OptTh.VanLaarhoven.Helpers.Comonad where

import OptTh.Prelude
import Control.Lens

import OptTh.VanLaarhoven.Types (VL, VL', CoVL, CoVL', ProVL, ProVL')

import Control.Comonad 
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader

----------------------------------------------------------------------------
------------------------------ ENV HELPERS ---------------------------------
----------------------------------------------------------------------------

ask' :: (ComonadEnv e w, MonadReader (w x) m) => Getting a e a -> m a
ask' l = Reader.asks (asks (view l))

asks' :: (ComonadEnv e w, MonadReader (w x) m) => VL' (Const r) e a -> (a -> r) -> m r
asks' l f = Reader.asks (asks (views l f))

local' :: ASetter e e' a b -> (a -> b) -> EnvT e w x -> EnvT e' w x
local' l f = local (over l f)

----------------------------------------------------------------------------
----------------------------- STORE HELPERS --------------------------------
----------------------------------------------------------------------------

pos' :: (ComonadStore e w, MonadReader (w x) m) => Getting a e a -> m a
pos' l = Reader.asks (view l . pos)

posMap :: (ComonadStore e w, MonadReader (w x) m) => VL' (Const r) e a -> (a -> r) -> m r
posMap l f = Reader.asks (views l f . pos)

peek' :: (ComonadStore e w) => ASetter e e a b -> b -> w x -> x
peek' l b = peeks (set l b)

peeks' :: (ComonadStore e w) => ASetter e e a b -> (a -> b) -> w x -> x
peeks' l f = peeks (over l f)

seek' :: (ComonadStore e w) => ASetter e e a b -> b -> w x -> w x
seek' l b = seeks (set l b)

seeks' :: (ComonadStore e w) => ASetter e e a b -> (a -> b) -> w x -> w x
seeks' l f = seeks (over l f)

experiment' :: (Functor f, ComonadStore e w) => VL f e e a b -> (a -> f b) -> w x -> f x
experiment' l f = experiment (l f)

----------------------------------------------------------------------------
----------------------------- TRACED HELPERS -------------------------------
----------------------------------------------------------------------------