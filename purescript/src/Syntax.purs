module Syntax where

import Prelude

import Data.Either (Either(Left))
import Data.Foreign (Foreign, ForeignError(JSONError), toForeign)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.List as List
import Data.List (List, (:), uncons)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple))

import Path (PathStep(..), toggle)

data Syntax
  = SyntaxNum Int
  | Plus Syntax Syntax
  | Hole String

derive instance genericSyntax :: Generic Syntax
instance showSyntax :: Show Syntax where show = gShow
instance eqSyntax :: Eq Syntax where eq = gEq

class MkForeign a where
  mkForeign :: a -> Foreign

instance syntaxMkForeign :: MkForeign Syntax where
  mkForeign (SyntaxNum i) = toForeign {tag: "number", value: i}
  mkForeign (Hole name) = toForeign {tag: "hole", name}
  mkForeign (Plus l r) = toForeign {tag: "plus", l: mkForeign l, r: mkForeign r}

instance syntaxIsForeign :: IsForeign Syntax where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "number" -> SyntaxNum <$> readProp "value" obj
      "plus" -> Plus <$> readProp "l" obj <*> readProp "r" obj
      "hole" -> Hole <$> readProp "name" obj
      _ -> Left (JSONError "found unexpected value in syntaxIsForeign")

newtype SyntaxZipper = SyntaxZipper {syntax :: Syntax, past :: Past}

type Past = List (Tuple PathStep Syntax)

makeZipper :: Syntax -> SyntaxZipper
makeZipper syntax = SyntaxZipper {syntax, past: List.Nil}

getTree :: SyntaxZipper -> Syntax
getTree (SyntaxZipper {syntax}) = syntax

getPast :: SyntaxZipper -> Past
getPast (SyntaxZipper {past}) = past

zipUp :: SyntaxZipper -> Syntax
zipUp = getTree <<< go
  where go tz = maybe tz go (up tz)

up :: SyntaxZipper -> Maybe SyntaxZipper
up (SyntaxZipper {syntax, past}) = do
  {head: Tuple dir graftSyn, tail} <- List.uncons past
  let newSyntax = case dir of
        StepLeft -> Plus syntax graftSyn
        StepRight -> Plus graftSyn syntax
  pure (SyntaxZipper {syntax: newSyntax, past: tail})

down :: PathStep -> SyntaxZipper -> Maybe SyntaxZipper
down dir (SyntaxZipper {syntax, past}) = case syntax of
  Plus l r -> Just case dir of
    StepLeft -> SyntaxZipper {syntax: l, past: Tuple StepLeft r : past}
    StepRight -> SyntaxZipper {syntax: r, past: Tuple StepRight l : past}
  _ -> Nothing

left :: SyntaxZipper -> Maybe SyntaxZipper
left zipper = do
  zipper' <- up zipper
  down StepLeft zipper'

right :: SyntaxZipper -> Maybe SyntaxZipper
right zipper = do
  zipper' <- up zipper
  down StepRight zipper'

sibling :: SyntaxZipper -> Maybe SyntaxZipper
sibling zipper = do
  {head: Tuple dir _} <- uncons (getPast zipper)
  zipper' <- up zipper
  down (toggle dir) zipper'

editFocus :: (Syntax -> Syntax) -> SyntaxZipper -> SyntaxZipper
editFocus f (SyntaxZipper {syntax, past}) = SyntaxZipper {syntax: f syntax, past}
