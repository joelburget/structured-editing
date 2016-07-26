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

-- class Sized a where
--   size :: a -> Int
--
-- instance syntaxSize :: Sized Syntax where

syntaxSize :: Syntax -> Int
syntaxSize (SyntaxNum n) = length (show n)
syntaxSize (Hole name) = length name
syntaxSize (Plus l r) = 5 + syntaxSize l + syntaxSize r

type SyntaxZipper =
  { syntax :: Syntax
  , past :: Past
  , anchor :: Path
  , focus :: Path
  }

newtype ZoomedSZ = ZoomedSZ SyntaxZipper

type Past = List (Tuple PathStep Syntax)

-- TODO answer question of whether this is zoomed. If the first shown character
-- is rendered by a child then it isn't.
makeZipper :: Syntax -> SyntaxZipper
makeZipper syntax = {syntax, past: List.Nil, anchor: PathOffset 0, focus: PathOffset 0}

zoomIn :: SyntaxZipper -> ZoomedSZ
zoomIn zipper@{syntax, past} = case syntax of
  SyntaxNum _ -> ZoomedSZ zipper
  Hole _ -> ZoomedSZ zipper
  Plus left right -> case tryZoomSelection zipper StepLeft of
    -- zoomIn (goLeft zipper)
    Just {anchor, focus} -> ZoomedSZ
      { syntax: left
      , past: Tuple StepLeft right : past
      , anchor
      , focus
      }
    -- TODO: dedup between this and goLeft / goRight
    -- zoomIn (goRight zipper)
    Nothing -> case tryZoomSelection zipper StepRight of
      Just {anchor, focus} -> ZoomedSZ
        { syntax: right
        , past: Tuple StepRight left : past
        , anchor
        , focus
        }
      Nothing -> ZoomedSZ zipper

getTree :: SyntaxZipper -> Syntax
getTree {syntax} = syntax

getPast :: SyntaxZipper -> Past
getPast {past} = past

zipUp :: SyntaxZipper -> SyntaxZipper
zipUp tz = maybe tz zipUp (up tz)

up :: SyntaxZipper -> Maybe SyntaxZipper
up {syntax, past, anchor, focus} = do
  {head: Tuple dir graftSyn, tail} <- List.uncons past
  -- graft the other side back on, track the size we're adding on the left so
  -- we can add it to the selection offsets
  let newSyntax = case dir of
        StepLeft -> Plus syntax graftSyn
        StepRight -> Plus graftSyn syntax
  pure
    { syntax: newSyntax
    , past: tail
    , anchor: PathCons dir anchor
    , focus: PathCons dir focus
    }

type ZipperSelection r = {anchor :: Path, focus :: Path | r}

tryZoomSelection :: forall a. ZipperSelection a
                 -> PathStep
                 -> Maybe (ZipperSelection ())
tryZoomSelection {anchor: PathCons a anchor, focus: PathCons f focus} step =
  if a == step && f == step then Just {anchor, focus} else Nothing
tryZoomSelection _ _  = Nothing

-- XXX get the anchor and focus right
down :: PathStep -> SyntaxZipper -> Maybe SyntaxZipper
down dir zipper@{syntax, past} = case syntax of
  Plus left right -> do
    {anchor, focus} <- tryZoomSelection zipper dir
    pure
      { syntax: left
      , past: Tuple StepLeft right : past
      , anchor
      , focus
      }
  _ -> Nothing

goLeft :: SyntaxZipper -> Maybe SyntaxZipper
goLeft zipper = do
  zipper' <- up zipper
  down StepLeft zipper'

goRight :: SyntaxZipper -> Maybe SyntaxZipper
goRight zipper = do
  zipper' <- up zipper
  down StepRight zipper'

sibling :: SyntaxZipper -> Maybe SyntaxZipper
sibling zipper = do
  {head: Tuple dir _} <- uncons (getPast zipper)
  zipper' <- up zipper
  down (toggle dir) zipper'

editFocus :: (Syntax -> Syntax) -> SyntaxZipper -> SyntaxZipper
editFocus f zipper@{syntax} = zipper {syntax = f syntax}


makePath :: Syntax -> Int -> Either String Path
makePath syntax n = lmap
  (\c -> "makePath overflow: " <>
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
