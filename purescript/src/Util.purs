module Util (isDigit, spliceStr, spliceArr, whenJust, iFor, iForM, forM, forM_, traceShowId, traceAnyId, traceLabelShowId, traceLabelAnyId) where

import Prelude
import Control.Monad.State (modify, get, evalState, evalStateT)
import Control.Monad.Trans (lift)
import Data.Maybe (Maybe(..))
import Data.Traversable

import Debug.Trace

-- yeah, i know this isn't a string function
isDigit :: Char -> Boolean
isDigit x = x >= '0' && x <= '9'

-- nor is this
whenJust :: forall a m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust val f = case val of
  Just val' -> f val'
  Nothing -> pure unit

foreign import spliceStr :: String -> Int -> Int -> String -> String
foreign import spliceArr :: forall a. Array a -> Int -> Int -> Array a -> Array a

iFor :: forall a b t. Traversable t => t a -> (Int -> a -> b) -> t b
iFor as f = flip evalState 0 $ flip traverse as \a -> do
  i <- get
  modify (_ + 1)
  pure (f i a)

iForM :: forall a b m t. (Traversable t, Monad m) => t a -> (Int -> a -> m b) -> m (t b)
iForM as f = flip evalStateT 0 $ flip traverse as \a -> do
  i <- get
  modify (_ + 1)
  lift $ f i a

forM :: forall a b m t. (Traversable t, Applicative m) => t a -> (a -> m b) -> m (t b)
forM = flip traverse

forM_ :: forall a b m t. (Traversable t, Applicative m) => t a -> (a -> m b) -> m Unit
forM_ x f = forM x f *> pure unit

traceShowId :: forall a. Show a => a -> a
traceShowId a = traceShow a \_ -> a

traceAnyId :: forall a. a -> a
traceAnyId a = traceAny a \_ -> a

traceLabelShowId :: forall a. Show a => String -> a -> a
traceLabelShowId label a = trace (label <> ": " <> show a) \_ -> a

traceLabelAnyId :: forall a. String -> a -> a
traceLabelAnyId label a = trace (label <> ": ") \_ -> traceAny a \_ -> a
