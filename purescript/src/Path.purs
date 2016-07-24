module Path where

import Prelude (class Eq, class Show, (==))
import Data.Generic
import Data.Maybe (Maybe(Just, Nothing))

data PathStep = StepLeft | StepRight

toggle :: PathStep -> PathStep
toggle StepLeft = StepRight
toggle StepRight = StepLeft

derive instance genericPathStep :: Generic PathStep
instance showPathStep :: Show PathStep where show = gShow
instance eqPathStep :: Eq PathStep where eq = gEq

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
