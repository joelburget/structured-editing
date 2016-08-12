module Path where

import Prelude
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(Just, Nothing))

type PathStep = Int

data CursorPath
  = CursorOutOfScope
  | PathOffset Int
  | PathCons PathStep CursorPath

derive instance genericPath :: Generic CursorPath
instance showPath :: Show CursorPath where show = gShow
instance eqPath :: Eq CursorPath where eq = gEq

-- TODO can't actually compare without knowing what language nodes these flow
-- through
-- instance ordPath :: Ord CursorPath where
--   compare (PathOffset x) (PathOffset y) = compare x y
--   compare (PathCons

pathsDifferOnlyInOffset :: CursorPath -> CursorPath -> Maybe {off1 :: Int, off2 :: Int}
pathsDifferOnlyInOffset (PathCons d1 rest1) (PathCons d2 rest2) =
  if d1 == d2 then pathsDifferOnlyInOffset rest1 rest2 else Nothing
pathsDifferOnlyInOffset (PathOffset off1) (PathOffset off2) = Just {off1, off2}
pathsDifferOnlyInOffset _ _ = Nothing

getOffset :: Maybe CursorPath -> Maybe Int
getOffset path = case path of
  Just (PathOffset n) -> Just n
  _ -> Nothing

subPath :: PathStep -> Maybe CursorPath -> Maybe CursorPath
subPath step path = case path of
  Nothing -> Nothing
  Just CursorOutOfScope -> Nothing
  Just (PathOffset _) -> Nothing
  Just (PathCons step' rest) ->
    if step == step'
       then Just rest
       else Nothing

pathHead :: CursorPath -> Maybe PathStep
pathHead (PathCons step _) = Just step
pathHead _ = Nothing

pathTail :: CursorPath -> Maybe CursorPath
pathTail (PathCons _ tail) = Just tail
pathTail _ = Nothing

pathUncons :: CursorPath -> Maybe {head :: PathStep, tail :: CursorPath}
pathUncons CursorOutOfScope = Nothing
pathUncons (PathOffset _) = Nothing
pathUncons (PathCons head tail) = Just {head, tail}

pathOffsetNum :: CursorPath -> Int -> CursorPath
pathOffsetNum CursorOutOfScope _ = CursorOutOfScope
pathOffsetNum (PathOffset i) j = PathOffset (i + j)
pathOffsetNum (PathCons step rest) j = PathCons step (pathOffsetNum rest j)

infix 4 pathOffsetNum as .+

data ConstraintStep
  = StepTy
  | StepChild PathStep

type ConstraintPath = Array ConstraintStep
