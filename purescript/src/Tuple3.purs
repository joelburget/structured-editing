module Tuple3 where

import Prelude (class Show, show, (<>))

data Tuple3 a b c = Tuple3 a b c

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show (Tuple3 a b c) = "(Tuple3 " <> show a <> " " <> show b <> " " <> show c <> ")"
