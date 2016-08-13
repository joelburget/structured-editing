module Syntax where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Except
import Data.Tuple
import Template
import Util
import Data.List as List
import Control.Monad.State (State, StateT, modify, get, put, evalState, evalStateT, runState)
import Data.Array as Array
import Data.Array.Partial (unsafeIndex)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(JSONError), readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Generic (Options, SumEncoding(..), defaultOptions, readGeneric)
import Data.Generic (class Generic, gShow, gEq)
import Data.List (List, uncons, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.String (length)
import Partial.Unsafe (unsafePartial)
import Path (NodePath, CursorPath(..), PathStep, pathHead, pathUncons)


throwE :: forall e m a. Applicative m => e -> ExceptT e m a
throwE = ExceptT <<< pure <<< Left

throwL :: forall e1 e2 m a. Applicative m => e1 -> ExceptT (Either e1 e2) m a
throwL = ExceptT <<< pure <<< Left <<< Left

throwR :: forall e1 e2 m a. Applicative m => e2 -> ExceptT (Either e1 e2) m a
throwR = ExceptT <<< pure <<< Left <<< Right


-- Note: This would seemingly make more sense as `class TemplatedTree syntax`,
-- but that would require `instance TemplatedTree (Syntax Internal Leaf)` for
-- each language -- ie an orphan instance, since those instances shouldn't be
-- defined in this module.
class TemplatedTree internal leaf where
  getLeafTemplate :: Syntax internal leaf -> String
  getInternalTemplate :: Syntax internal leaf -> Template

class Operational internal leaf where
  doOperate
    :: SyntaxZipper internal leaf
    -> Action
    -> Either String (SyntaxZipper internal leaf)

class (TemplatedTree internal leaf) <= Lang internal leaf where
  -- this is (right now) a limited notion of evaluation. i'd love to express a
  -- small-step semantics rather than a big-step all-at-once evaluation
  normalize :: Syntax internal leaf -> Syntax internal leaf

  propagateUpType
    :: SyntaxZipper internal leaf
    -> Syntax internal leaf
    -> SyntaxZipper internal leaf

  propagateDownType
    :: {term :: Syntax internal leaf, outsideTy :: Syntax internal leaf}
    -> Syntax internal leaf

  infer :: Syntax internal leaf -> Syntax internal leaf

type ConflictInfo internal leaf =
  { term :: Syntax internal leaf
  , outsideTy :: Syntax internal leaf
  , insideTy :: Syntax internal leaf
  }

-- TODO decide whether some of these cases should be merged. Fundamentally this
-- is a labelled rose tree and we don't necessarily need to distinguish between
-- the cases. Also could be `CoFree`.
data Syntax internal leaf
  -- We allow `Nothing` here *only* in the case we're looking inside a conflict
  = Internal internal (Array (Syntax internal leaf))
  | Leaf leaf
  | Hole String
  | Conflict
    -- weirdly this has to be inlined for generic deriving even though it's
    -- really just ConflictInfo
    { term :: Syntax internal leaf
    , outsideTy :: Syntax internal leaf
    , insideTy :: Syntax internal leaf
    }

derive instance genericSyntax :: (Generic a, Generic b) => Generic (Syntax a b)
instance showSyntax :: (Generic a, Generic b) => Show (Syntax a b) where show = gShow
instance eqSyntax :: (Generic a, Generic b) => Eq (Syntax a b) where eq = gEq

instance syntaxIsForeign :: (IsForeign a, IsForeign b) => IsForeign (Syntax a b) where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "internal" -> do
         Internal <$> readProp "value" obj <*> readProp "children" obj
      "leaf" -> Leaf <$> readProp "value" obj
      "hole" -> Hole <$> readProp "value" obj
      "conflict" -> do
        term <-readProp "term" obj
        outsideTy <- readProp "outsideTy" obj
        insideTy <- readProp "insideTy" obj
        pure (Conflict {term, outsideTy, insideTy})
      _ -> Left (JSONError "found unexpected value in syntaxIsForeign")

syntaxHoles :: forall a b. Syntax a b -> Array (Syntax a b)
syntaxHoles (Internal _ children) = children >>= syntaxHoles
syntaxHoles (Leaf _) = []
syntaxHoles h@(Hole _) = [h]
syntaxHoles (Conflict {term}) = syntaxHoles term

syntaxConflicts
  :: forall a b. Syntax a b
  -> Array {conflict :: Syntax a b, loc :: NodePath}
syntaxConflicts (Internal _ children) =
  let conflictsPerChild = map syntaxConflicts children
      childIndexedConflicts = mapWithIndex
        (\i childConflicts -> map
          (\{conflict, loc} -> {conflict, loc: i Array.: loc})
          childConflicts
        )
        conflictsPerChild
  in join childIndexedConflicts

syntaxConflicts (Leaf _) = []
syntaxConflicts (Hole _) = []
syntaxConflicts conflict@(Conflict _) = [{conflict, loc: []}]

type SyntaxZipper a b =
  { syntax :: Syntax a b
  -- TODO past becomes a bad name as we (maybe) generalize this zipper to undo
  -- / redo as well.
  , past :: Past a b
  , anchor :: CursorPath
  , focus :: CursorPath
  }

newtype ZoomedSZ a b = ZoomedSZ (SyntaxZipper a b)

data ZipperValue a
  = InternalValue a
  | ConflictValue

derive instance genericZipperValue :: Generic a => Generic (ZipperValue a)
instance showZipperValue :: Generic a => Show (ZipperValue a) where show = gShow
instance eqZipperValue :: Generic a => Eq (ZipperValue a) where eq = gEq

type ZipperStep a b =
  { value :: ZipperValue a
  , otherChildren :: Array (Syntax a b)
  , dir :: PathStep
  }

type Past a b = List (ZipperStep a b)

-- TODO answer question of whether this is zoomed. If the first shown character
-- is rendered by a child then it isn't.
makeZipper :: forall a b. Syntax a b -> SyntaxZipper a b
makeZipper syntax = {syntax, past: List.Nil, anchor: PathOffset 0, focus: PathOffset 0}

moveTo :: forall a b. SyntaxZipper a b -> NodePath -> SyntaxZipper a b
moveTo z steps = case Array.uncons steps of
  Just {head, tail} ->
    let subZ = case down head z of
          Just z' -> z'
          -- XXX what if the bookmark went away because it was resolved?
          Nothing -> z -- unsafeThrow "invariant violation: moveTo couldn't move down"
    in moveTo subZ tail
  Nothing -> z

getStepsFromRoot :: forall a b. Past a b -> NodePath
getStepsFromRoot = Array.reverse <<< Array.fromFoldable <<< map _.dir

type Bookmark =
  { anchor :: CursorPath
  , focus :: CursorPath
  , zoom :: NodePath
  }

bookmark :: forall a b. SyntaxZipper a b -> Bookmark
bookmark z@{anchor, focus, past} = {anchor, focus, zoom: getStepsFromRoot past}

conflictIx :: forall a b. Int
           -> Syntax a b
           -> {value :: Syntax a b, otherChildren :: Array (Syntax a b)}
conflictIx i (Conflict {term, outsideTy, insideTy}) = case i of
  0 -> {value: term, otherChildren: [outsideTy, insideTy]}
  1 -> {value: outsideTy, otherChildren: [term, insideTy]}
  2 -> {value: insideTy, otherChildren: [term, outsideTy]}
  n -> unsafeThrow $ "unexpected index into Conflict: " <> show n
conflictIx _ tm = unsafeThrow "unexpected conflictIx"

zoomIn :: forall a b. SyntaxZipper a b -> ZoomedSZ a b
zoomIn zipper@{syntax, past, anchor, focus} = case syntax of
  Leaf _ -> ZoomedSZ zipper
  Hole _ -> ZoomedSZ zipper
  Conflict _ ->
    if pathHead anchor == pathHead focus
    then
      let go = do
            {head: dir, tail: aTail} <- pathUncons anchor
            {tail: fTail} <- pathUncons focus
            let children = conflictIx dir syntax
            pure $ zoomIn
              { syntax: children.value
              , past:
                { value: ConflictValue
                , otherChildren: children.otherChildren
                , dir
                } : past
              , anchor: aTail
              , focus: fTail
              }
      in case go of
           Just zoomed -> zoomed
           Nothing -> ZoomedSZ zipper
    else ZoomedSZ zipper
  Internal value children ->
    if pathHead anchor == pathHead focus
    then
      let go = do
            {head: dir, tail: aTail} <- pathUncons anchor
            {tail: fTail} <- pathUncons focus
            pure $ zoomIn
              { syntax: unsafePartial (unsafeIndex children dir)
              , past:
                { value: InternalValue value
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
zipUp tz = maybe tz zipUp (_.zipper <$> up tz)

up :: forall a b. SyntaxZipper a b
   -> Maybe {zipper :: SyntaxZipper a b, prevLoc :: PathStep}
up {syntax, past, anchor, focus} = do
  {head: {dir, otherChildren, value}, tail} <- List.uncons past
  -- graft the other side back on, track the size we're adding on the left so
  -- we can add it to the selection offsets
  let children = spliceArr otherChildren dir 0 [syntax]
      newSyntax = case value of
        InternalValue value' -> Internal value' children
        ConflictValue -> case children of
          [term, outsideTy, insideTy] -> Conflict {term, outsideTy, insideTy}
          _ -> unsafeThrow "deeply broken up"
  let zipper =
        { syntax: newSyntax
        , past: tail
        , anchor: PathCons dir anchor
        , focus: PathCons dir focus
        }
  pure {zipper, prevLoc: dir}

type ZipperSelection a b r = {anchor :: CursorPath, focus :: CursorPath | r}

zoomSelection
  :: forall a b c. ZipperSelection a b c
  -> PathStep
  -> ZipperSelection a b ()
zoomSelection {anchor, focus} step =
  let anchor'' = case anchor of
        PathCons a anchor' -> if a == step then anchor' else CursorOutOfScope
        _ -> CursorOutOfScope
      focus'' = case focus of
        PathCons a focus' -> if a == step then focus' else CursorOutOfScope
        _ -> CursorOutOfScope
   in {anchor: anchor'', focus: focus''}

-- XXX get the anchor and focus right
down :: forall a b. PathStep -> SyntaxZipper a b -> Maybe (SyntaxZipper a b)
down dir zipper@{syntax, past} = case syntax of
  Conflict _ -> case conflictIx dir syntax of
    {value, otherChildren} -> do
      let zoomed = zoomSelection zipper dir
      pure
        -- XXX unsafe
        { syntax: value
        , past: {dir, otherChildren, value: ConflictValue} : past
        , anchor: zoomed.anchor
        , focus: zoomed.focus
        }
  Internal value children -> do
    let otherChildren = spliceArr children dir 1 []
        zoomed = zoomSelection zipper dir
    pure
      -- XXX unsafe
      { syntax: unsafePartial (unsafeIndex children dir)
      , past: {dir, otherChildren, value: InternalValue value} : past
      , anchor: zoomed.anchor
      , focus: zoomed.focus
      }
  x -> Nothing

editFocus
  :: forall a b. (Syntax a b -> Syntax a b)
  -> SyntaxZipper a b
  -> SyntaxZipper a b
editFocus f zipper@{syntax} = zipper {syntax = f syntax}


makePath
  :: forall a b. TemplatedTree a b
  => Syntax a b
  -> Int
  -> Either String CursorPath
makePath syntax n =
  let z = (runExceptT (consumePath syntax))
      y :: Tuple (Either (Either String CursorPath) Unit) Int
      y = runState z n
  in case y of
       Tuple (Left x) _ -> x
       Tuple (Right _) c -> Left $ "makePath overflow: " <>
         show n <> " demanded, " <> show c <> " remaining"

newtype ConsumedInPreviousChunks = ConsumedInPreviousChunks Int

-- keep moving in to the outermost syntax holding this offset
-- either give back the resulting path or the number of chars
-- consumed
consumePath :: forall a b. TemplatedTree a b
            => Syntax a b
            -> ExceptT (Either String CursorPath) (State Int) Unit
consumePath l@(Leaf _) = do
  let str = getLeafTemplate l
  offset <- get
  let len = length str
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

consumePath i@(Internal _ children) = do
  let template = getInternalTemplate i
  arr <- case zipTemplate template children of
           Nothing -> throwL "couldn't zip!"
           Just arr' -> pure arr'

  -- state monad to track how much we've consumed
  -- except to give back result
  -- TODO I must be doing something wrong to have to do all this lifting...
  let yieldsPiece :: StateT ConsumedInPreviousChunks
                     (ExceptT (Either String CursorPath)
                     (State Int))
                     Unit
      yieldsPiece = forM_ arr \child -> case child of
        Left str -> do
          let len = length str
          (ConsumedInPreviousChunks prev) <- get
          offset <- lift $ lift $ get
          if offset < len
            then lift $ throwR (PathOffset (prev + offset))
            else do lift $ lift $ modify (_ - len)
                    put $ ConsumedInPreviousChunks (prev + len)
        Right (Tuple childSyn ix) ->
          lift $ withExceptT (rmap (PathCons ix)) (consumePath childSyn)

  evalStateT yieldsPiece (ConsumedInPreviousChunks 0)

  pure unit

consumePath (Conflict {term}) =
  withExceptT (rmap (PathCons 0)) (consumePath term)


data Action
  = Backspace
  | Typing Char
  | TakeInside NodePath
  | TakeOutside NodePath
  | Tab { shift :: Boolean }

derive instance genericAction :: Generic Action
instance showAction :: Show Action where show = gShow
instance eqAction :: Eq Action where eq = gEq

-- "expected one of ["Main.Backspace","Main.Typing"]"
-- instance foreignAction :: IsForeign Action where
--   read = readGeneric myOptions

instance actionIsForeign :: IsForeign Action where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "typing" -> Typing <$> readProp "value" obj
      "backspace" -> pure Backspace
      "take-outside" -> TakeOutside <$> readProp "loc" obj
      "take-inside" -> TakeInside <$> readProp "loc" obj
      "tab" -> do
        shift <- readProp "shift" obj
        pure $ Tab {shift}
      _ -> Left (JSONError "found unexpected value in actionIsForeign")

