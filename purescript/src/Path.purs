module Path where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(Just, Nothing))

type PathStep = Int

data Path
  = PathOffset Int
  | PathCons PathStep Path

derive instance genericPath :: Generic Path
instance showPath :: Show Path where show = gShow
instance eqPath :: Eq Path where eq = gEq

getOffset :: Maybe Path -> Maybe Int
getOffset path = case path of
  Just (PathOffset n) -> Just n
  _ -> Nothing

subPath :: PathStep -> Maybe Path -> Maybe Path
subPath step path = case path of
  Nothing -> Nothing
  Just (PathOffset _) -> Nothing
  Just (PathCons step' rest) ->
    if step == step'
       then Just rest
       else Nothing

pathHead :: Path -> Maybe PathStep
pathHead (PathCons step _) = Just step
pathHead _ = Nothing

pathTail :: Path -> Maybe Path
pathTail (PathCons _ tail) = Just tail
pathTail _ = Nothing

pathOffsetNum :: Path -> Int -> Path
pathOffsetNum (PathOffset i) j = PathOffset (i + j)
pathOffsetNum (PathCons step rest) j = PathCons step (pathOffsetNum rest j)

infix 4 pathOffsetNum as .+
