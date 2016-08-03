module Syntax where

import Prelude
import Control.Monad.Except
import Data.Tuple
import Template
import Util.String
import Data.List as List
import Control.Monad.State (State, modify, get, evalState, evalStateT, runState)
import Data.Array.Partial (unsafeIndex)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(JSONError), readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Generic (Options, SumEncoding(..), defaultOptions, readGeneric)
import Data.Generic (class Generic, gShow, gEq)
import Data.List (List, (:), uncons)
import Data.Maybe (Maybe(..), maybe)
import Data.String (length)
import Partial.Unsafe (unsafePartial)
import Path (Path(..), PathStep, pathHead, pathUncons)


-- "syntax unit"
data SUnit = SUnit
derive instance genericSUnit :: Generic SUnit
instance showSUnit :: Show SUnit where show = gShow
instance eqSUnit :: Eq SUnit where eq = gEq
instance foreignSUnit :: IsForeign SUnit where read _ = pure SUnit

throwE :: forall e m a. Applicative m => e -> ExceptT e m a
throwE = ExceptT <<< pure <<< Left

throwL :: forall e1 e2 m a. Applicative m => e1 -> ExceptT (Either e1 e2) m a
throwL = ExceptT <<< pure <<< Left <<< Left

throwR :: forall e1 e2 m a. Applicative m => e2 -> ExceptT (Either e1 e2) m a
throwR = ExceptT <<< pure <<< Left <<< Right

data Internal
  = IfThenElse
  | Addition
  | Parens
  | Eq

derive instance genericInternal :: Generic Internal
instance showInternal :: Show Internal where show = gShow
instance eqInternal :: Eq Internal where eq = gEq
instance internalIsForeign :: IsForeign Internal where
  read obj = do
    tag <- readString obj
    case tag of
      "ifthenelse" -> pure IfThenElse
      "addition" -> pure Addition
      "parens" -> pure Parens
      "eq" -> pure Eq
      _ -> Left (JSONError "found unexpected value in internalIsForeign")

data Leaf
  = BoolLeaf Boolean
  | IntLeaf Int

derive instance genericLeaf :: Generic Leaf
instance showLeaf :: Show Leaf where show = gShow
instance eqLeaf :: Eq Leaf where eq = gEq

instance leafIsForeign :: IsForeign Leaf where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "bool" -> BoolLeaf <$> readProp "value" obj
      "int" -> IntLeaf <$> readProp "value" obj
      _ -> Left (JSONError "found unexpected value in leafIsForeign")

data Syntax internal leaf
  = Internal internal (Array (Syntax internal leaf))
  | Leaf leaf
  | Hole String
  | Conflict (Syntax internal leaf) (Syntax internal leaf)

derive instance genericSyntax :: (Generic a, Generic b) => Generic (Syntax a b)
instance showSyntax :: (Generic a, Generic b) => Show (Syntax a b) where show = gShow
instance eqSyntax :: (Generic a, Generic b) => Eq (Syntax a b) where eq = gEq

instance syntaxIsForeign :: (IsForeign a, IsForeign b) => IsForeign (Syntax a b) where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "internal" -> Internal <$> readProp "value" obj <*> readProp "children" obj
      "leaf" -> Leaf <$> readProp "value" obj
      "hole" -> Hole <$> readProp "value" obj
      "conflict" -> Conflict <$> readProp "left" obj <*> readProp "right" obj
      _ -> Left (JSONError "found unexpected value in syntaxIsForeign")

-- zipper and zoomed zipper for this particular language
type LangSyntax = Syntax Internal Leaf
type LangZipper = SyntaxZipper Internal Leaf
type ZoomedLang = ZoomedSZ Internal Leaf

syntaxHoles :: forall a b. Syntax a b -> Array String
syntaxHoles (Internal _ children) = children >>= syntaxHoles
syntaxHoles (Leaf _) = []
syntaxHoles (Hole name) = [name]
syntaxHoles (Conflict _ _) = []

type SyntaxZipper a b =
  { syntax :: Syntax a b
  , past :: Past a b
  , anchor :: Path
  , focus :: Path
  }

newtype ZoomedSZ a b = ZoomedSZ (SyntaxZipper a b)

type ZipperStep a b = {value :: a, otherChildren :: Array (Syntax a b), dir :: PathStep}

type Past a b = List (ZipperStep a b)

-- TODO answer question of whether this is zoomed. If the first shown character
-- is rendered by a child then it isn't.
makeZipper :: forall a b. Syntax a b -> SyntaxZipper a b
makeZipper syntax = {syntax, past: List.Nil, anchor: PathOffset 0, focus: PathOffset 0}

followPath :: forall a b. Syntax a b -> Path -> Syntax a b
followPath syntax path = case syntax of
  Leaf _ -> syntax
  Hole _ -> syntax
  Conflict _ _ -> syntax
  Internal value children -> case path of
    PathCons dir tail -> followPath (unsafePartial (unsafeIndex children dir)) tail
    PathOffset _ -> syntax

zoomIn :: forall a b. SyntaxZipper a b -> ZoomedSZ a b
zoomIn zipper@{syntax, past, anchor, focus} = case syntax of
  Leaf _ -> ZoomedSZ zipper
  Hole _ -> ZoomedSZ zipper
  Conflict _ _ -> ZoomedSZ zipper
  Internal value children ->
    if pathHead anchor == pathHead focus
    then
      let go = do
            {head: dir, tail: aTail} <- pathUncons anchor
            {tail: fTail} <- pathUncons focus
            pure $ zoomIn
              { syntax: unsafePartial (unsafeIndex children dir)
              , past:
                { value
                , otherChildren: spliceArr children dir 1 []
                , dir
                } : past
              , anchor: aTail
              , focus: fTail
              }
      in case go of
           Just zoomed -> zoomed
           Nothing -> ZoomedSZ zipper
    else ZoomedSZ zipper

getTree :: forall a b. SyntaxZipper a b -> Syntax a b
getTree {syntax} = syntax

getPast :: forall a b. SyntaxZipper a b -> Past a b
getPast {past} = past

zipUp :: forall a b. SyntaxZipper a b -> SyntaxZipper a b
zipUp tz = maybe tz zipUp (up tz)

up :: forall a b. SyntaxZipper a b -> Maybe (SyntaxZipper a b)
up {syntax, past, anchor, focus} = do
  {head: {dir, otherChildren, value}, tail} <- List.uncons past
  -- graft the other side back on, track the size we're adding on the left so
  -- we can add it to the selection offsets
  let children = spliceArr otherChildren dir 0 [syntax]
      newSyntax = Internal value children
  pure
    { syntax: newSyntax
    , past: tail
    , anchor: PathCons dir anchor
    , focus: PathCons dir focus
    }

type ZipperSelection a b r = {anchor :: Path, focus :: Path | r}

tryZoomSelection :: forall a b c. ZipperSelection a b c
                 -> PathStep
                 -> Maybe (ZipperSelection a b ())
tryZoomSelection {anchor: PathCons a anchor, focus: PathCons f focus} step =
  if a == step && f == step then Just {anchor, focus} else Nothing
tryZoomSelection _ _  = Nothing

-- XXX get the anchor and focus right
down :: forall a b. PathStep -> SyntaxZipper a b -> Maybe (SyntaxZipper a b)
down dir zipper@{syntax, past} = case syntax of
  Internal value children -> do
    let otherChildren = spliceArr children dir 1 []
    {anchor, focus} <- tryZoomSelection zipper dir
    pure
      -- XXX unsafe
      { syntax: unsafePartial (unsafeIndex children dir)
      , past: {dir, otherChildren, value} : past
      , anchor
      , focus
      }
  _ -> Nothing

editFocus :: forall a b. (Syntax a b -> Syntax a b) -> SyntaxZipper a b -> SyntaxZipper a b
editFocus f zipper@{syntax} = zipper {syntax = f syntax}


-- makePath :: forall a b. Show b => Syntax a b -> Int -> Either String Path
makePath :: LangSyntax -> Int -> Either String Path
makePath syntax n =
  let z = (runExceptT (consumePath syntax))
      y :: Tuple (Either (Either String Path) Unit) Int
      y = runState z n
  in case y of
       Tuple (Left x) _ -> x
       Tuple (Right _) c -> Left $ "makePath overflow: " <>
         show n <> " demanded, " <> show c <> " consumed"


-- keep moving in to the outermost syntax holding this offset
-- either give back the resulting path or the number of chars
-- consumed
-- consumePath :: forall a b. Show b
--             => Syntax a b
--             -> ExceptT (Either String Path) (State Int) Unit
consumePath :: LangSyntax
            -> ExceptT (Either String Path) (State Int) Unit
consumePath (Leaf l) = do
  let template = case l of
        BoolLeaf b -> show b
        IntLeaf i -> show i
  offset <- get
  let len = length template
  if offset <= len
     then throwR (PathOffset offset)
     else modify (_ - len)

-- TODO pretty much identical to prev -- reduce duplication
consumePath (Hole name) = do
  offset <- get
  let namelen = length name
  if offset <= namelen
     then throwR (PathOffset offset)
     else modify (_ - namelen)

consumePath (Internal tag children) = do
  let template = case tag of
        IfThenElse -> ifThenElseTemplate
        Addition -> additionTemplate
        Parens -> parensTemplate
        Eq -> eqTemplate
  arr <- case zipTemplate template children of
           Nothing -> throwL "couldn't zip!"
           Just arr -> pure arr

  -- state monad to track how much we've consumed
  -- except to give back result
  forM arr \child -> case child of
    Left str -> do
      let len = length str
      offset <- get
      if offset <= len
        then throwR (PathOffset offset)
        else modify (_ - len)
    Right (Tuple childSyn ix) ->
      withExceptT (rmap (PathCons ix)) (consumePath childSyn)

  pure unit

-- {[left] vs [right]}
consumePath (Conflict left right) = do
  let checkAndModify str = do
        let len = length str
        i <- get
        when (i <= len) (throwR (PathOffset i))
        modify (_ + len) -- "}"

  checkAndModify "}"
  withExceptT (rmap (PathCons 0)) (consumePath left)
  checkAndModify " vs "
  withExceptT (rmap (PathCons 1)) (consumePath right)
  checkAndModify "}"
