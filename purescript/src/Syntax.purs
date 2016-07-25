module Syntax where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.List as List
import Data.List (List, (:), uncons)
import Data.Maybe (Maybe(..), maybe)
import Data.String (length)
import Data.Tuple (Tuple(Tuple))

import Path (Path(..), PathStep(..), toggle)

data Syntax
  = SyntaxNum Int
  | Plus Syntax Syntax
  | Hole String

derive instance genericSyntax :: Generic Syntax
instance showSyntax :: Show Syntax where show = gShow
instance eqSyntax :: Eq Syntax where eq = gEq

instance syntaxIsForeign :: IsForeign Syntax where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "number" -> SyntaxNum <$> readProp "value" obj
      "plus" -> Plus <$> readProp "l" obj <*> readProp "r" obj
      "hole" -> Hole <$> readProp "name" obj
      _ -> Left (JSONError "found unexpected value in syntaxIsForeign")

syntaxSize :: Syntax -> Int
syntaxSize (SyntaxNum n) = length (show n)
syntaxSize (Hole name) = length name
syntaxSize (Plus l r) = 5 + syntaxSize l + syntaxSize r

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


makePath :: Syntax -> Int -> Either String Path
makePath syntax n = lmap
  (\c -> "rawSelectionToSelection overflow: " <>
    show n <> " demanded, " <> show c <> " consumed")
  (consumePath syntax n)
  where
    -- keep moving in to the outermost syntax holding this offset
    -- either give back the resulting path or the number of chars
    -- consumed
    consumePath :: Syntax -> Int -> Either Int Path
    consumePath _ 0 = Right (PathOffset 0)
    consumePath (SyntaxNum n) offset =
      let nlen = length (show n)
      in if offset <= nlen
         then Right (PathOffset offset)
         else Left nlen

    -- pretty much identical
    consumePath (Hole name) offset =
      let namelen = length name
      in if offset <= namelen
         then Right (PathOffset offset)
         else Left namelen

    -- ([left] + [right])
    --
    -- TODO automate this kind of offset calculation
    consumePath (Plus l r) offset =
      case consumePath l (offset - 1) of
        Right path -> Right (PathCons StepLeft path)
        Left consumed ->
          let offset' = offset - (consumed + 1)
          in if offset' < 3
             then Right (PathOffset (offset' + 1))
             else case consumePath r (offset' - 3) of
                    Right path -> Right (PathCons StepRight path)
                    Left consumed' ->
                      let subConsumed = consumed + consumed'
                      in if offset - subConsumed <= 5
                         then Right (PathOffset (offset - subConsumed))
                         else Left (subConsumed + 5)

unmakePath :: Syntax -> Path -> Either String Int
unmakePath (SyntaxNum n) (PathOffset o) = if o <= length (show n)
  then Right o
  else Left "unmakePath: offset too large for number"
unmakePath (SyntaxNum _) (PathCons _ _) =
  Left "unmakePath: tried to go in to number"
unmakePath (Hole name) (PathOffset o) = if o <= length name
  then Right o
  else Left "unmakePath: offset too large for hole name"
unmakePath (Hole _) (PathCons _ _) =
  Left "unmakePath: tried to go in to hole"
unmakePath (Plus l r) (PathOffset o) =
  if o == 0
    then Right 0
    else if o <= 3
      then Right (syntaxSize l + o)
      else if o <= 5
        then Right (syntaxSize l + syntaxSize r + o)
        else Left "unmakePath: offste too large for plus"
unmakePath (Plus l r) (PathCons dir rest) =
  if dir == StepLeft
    then (1 + _) <$> unmakePath l rest
    -- else (4 + (syntaxSize l + _)) <$> unmakePath r rest
    else do
      i <- unmakePath r rest
      pure (4 + syntaxSize l + i)
