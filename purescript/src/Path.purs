module Path where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(Just, Nothing))

type PathStep = Int

-- TODO this is more accurately a `DisplayPath` -- it allows offsets into a
-- leaf and it only goes top to bottom (no stepping up)
data Path
  = PathOffset Int
  | PathCons PathStep Path

derive instance genericPath :: Generic Path
instance showPath :: Show Path where show = gShow
instance eqPath :: Eq Path where eq = gEq

-- TODO can't actually compare without knowing what language nodes these flow
-- through
-- instance ordPath :: Ord Path where
--   compare (PathOffset x) (PathOffset y) = compare x y
--   compare (PathCons

pathsDifferOnlyInOffset :: Path -> Path -> Maybe {off1 :: Int, off2 :: Int}
pathsDifferOnlyInOffset (PathCons d1 rest1) (PathCons d2 rest2) =
  if d1 == d2 then pathsDifferOnlyInOffset rest1 rest2 else Nothing
pathsDifferOnlyInOffset (PathOffset off1) (PathOffset off2) = Just {off1, off2}
pathsDifferOnlyInOffset _ _ = Nothing

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

pathUncons :: Path -> Maybe {head :: PathStep, tail :: Path}
pathUncons (PathOffset _) = Nothing
pathUncons (PathCons head tail) = Just {head, tail}

pathOffsetNum :: Path -> Int -> Path
pathOffsetNum (PathOffset i) j = PathOffset (i + j)
pathOffsetNum (PathCons step rest) j = PathCons step (pathOffsetNum rest j)

infix 4 pathOffsetNum as .+

data ConstraintStep
  = StepTy
  | StepChild PathStep

type ConstraintPath = Array ConstraintStep
