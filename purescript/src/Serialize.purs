-- | Here's where we serialize from our internal (zipper) representation to
-- | draft's flat represenation.
module Serialize where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (snoc, (..), (:))
import Data.List as List
import Data.Map as Map
import Data.String as String
import Control.Monad.State (State, modify, get, evalState, execState)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Generic (readGeneric)
import Data.Function.Uncurried (mkFn2, Fn2, mkFn1, Fn1)
import Data.Generic (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (sequence)
import Generic (myOptions)

import Template (DraftInline, DraftInlineType(..), InlineInfo, interpolateTemplate, inlineSelection)
import Path (CursorPath, NodePath, subPath, getOffset)
import Syntax (class Lang, Syntax(Conflict, Hole, Leaf, Internal), ZoomedSZ(ZoomedSZ), normalize, zoomIn, syntaxHoles, syntaxConflicts, zipUp, makePath, getLeafTemplate, getInternalTemplate)
import Util (whenJust, iForM)


type EntityRange =
  { offset :: Int
  , length :: Int
  , key :: Int
  }

newtype BlockKey = BlockKey String
newtype EntityKey = EntityKey String

type RawDraftContentBlock =
  { key :: BlockKey
    -- type :: 'unstyled'
  , text :: String
  , entityRanges :: Array EntityRange
  }

newtype PreEntityMap = PreEntityMap (Maybe (Array String))
instance showPEM :: Show PreEntityMap where
  show (PreEntityMap m) = show m


type ContentState =
  { block :: RawDraftContentBlock
  -- TODO understand selectionBefore / selectionAfter
  , selection :: RawSelection
  , preEntityMap :: PreEntityMap
  }

blockKey :: String
blockKey = "editorblockkey"

-- internal "block from content" accumulator state
type BFromCState =
  { currentOffset :: Int
  -- XXX something more descriptive than String?
  , anchorKey :: String
  , anchorOffset :: Int
  , focusKey :: String
  , focusOffset :: Int
  , text :: String
  , entityRanges :: Array EntityRange
  , preEntityMap :: Map Int String
  }


-- | `blockFromContent` transforms the intermediate representation yielded by
-- | `contentFromSyntax` into the final `ContentState` representation that
-- | draft.js deals with.
blockFromContent :: Array DraftInline -> ContentState
blockFromContent inlines =
  let initialState =
        { currentOffset: 0
        , anchorKey: blockKey
        , anchorOffset: 0
        , focusKey: blockKey
        , focusOffset: 0
        , text: ""
        , entityRanges: []
        , preEntityMap: Map.empty
        }

      accum :: BFromCState -> DraftInline -> BFromCState
      accum state {ty, key, content, info} = case ty of
        InlineInternal -> accumText key "internal" content (accumAnchorFocus info state)
        InlineLeaf -> accumText key "leaf" content (accumAnchorFocus info state)
        InlineHole -> accumText key "hole" content (accumAnchorFocus info state)
        InlineConflict -> accumText key "conflict" content (accumAnchorFocus info state)

      -- For each DraftInline, add its text to the current state
      accumText :: Int -> String -> String -> BFromCState -> BFromCState
      accumText key entityType str state =
        let offset = String.length state.text
            sLen = String.length str
        in state
             { currentOffset = state.currentOffset + sLen
             , text = state.text <> str
             , entityRanges = snoc state.entityRanges
               { offset
               , length: sLen
               , key
               }
               , preEntityMap = Map.insert key entityType state.preEntityMap
             }

      accumAnchorFocus :: InlineInfo -> BFromCState -> BFromCState
      accumAnchorFocus info state =
        -- TODO use lens
        -- whenJust info.anchor \i ->
        --   anchorOffset %=
        --   anchorKey .=
        let go = do
              whenJust info.anchor \i -> modify \st -> st
                { anchorOffset = state.currentOffset + i
                , anchorKey = blockKey
                }
              whenJust info.focus \i -> modify \st -> st
                { focusOffset = state.currentOffset + i
                , focusKey = blockKey
                }
        in execState go state

      finalState = foldl accum initialState inlines
  in { block: { key: BlockKey blockKey
              , text: finalState.text
              , entityRanges: finalState.entityRanges
              }
     , selection: { anchorKey: finalState.anchorKey
                  , anchorOffset: finalState.anchorOffset
                  , focusKey: finalState.focusKey
                  , focusOffset: finalState.focusOffset
                  }
     , preEntityMap: toPreEntityMap finalState.preEntityMap
     }


-- | Serialize syntax into an array of spans and a mapping from `Int` ids to
-- | the path to reach the identified node.
-- |
-- | Each node in the tree gets a uniquely generated id. This id is attached to
-- | each `DraftInline`. This way it's trivial to take any given character,
-- | find its id, and map that to a `NodePath` to find it in the tree.
-- |
-- | TODO(joel) disambiguate whether this path is from the root of the tree.
-- | This should be the case for `genContentState` but I'm not sure about
-- | `genDisplayContentState`.
contentFromSyntax
  :: forall a b. (Lang a b)
  => (Syntax a b)
  -- TODO change the Nothing case to CursorOutOfScope?
  -> CursorPath
  -> CursorPath
  -> State Int {inlines :: Array DraftInline, ids :: Map Int NodePath}
contentFromSyntax (Conflict {term, insideTy, outsideTy}) anchor focus = do
  {inlines, ids} <- contentFromSyntax term (subPath 0 anchor) (subPath 0 focus)
  let inlines' = map (\li -> li { ty = InlineConflict }) inlines
  pure {inlines: inlines', ids}

contentFromSyntax syntax anchor focus = do
  let anchorOffset = getOffset anchor
      focusOffset = getOffset focus
  myId <- get
  modify (_ + 1)
  case syntax of
    Internal tag children -> do
      let template = getInternalTemplate syntax
      -- TODO -- returning this way causes us to traverse the array twice later
      children' <- iForM children \i child -> do
        {inlines, ids: childIds} <- contentFromSyntax child (subPath i anchor) (subPath i focus)
        pure
          { inlines
          , ids: (i:_) <$> childIds
          }

      -- XXX should be able to actually fail here
      let preInlines = interpolateTemplate template InlineInternal {key: myId, anchorOffset, focusOffset} (map _.inlines children')
          inlines = maybe [] id preInlines

      let ids = Map.insert myId [] (Map.unions (map _.ids children'))
      pure {inlines, ids}

    Leaf l ->
      let content = getLeafTemplate syntax
          inlines = [
            {ty: InlineLeaf, key: myId, content, info: inlineSelection 0 (String.length content) anchorOffset focusOffset}
          ]
          ids = Map.singleton myId []
      in pure {inlines, ids}

    Hole str ->
      let inlines = [
          {ty: InlineHole, key: myId, content: str, info: inlineSelection 0 (String.length str) anchorOffset focusOffset}
          ]
          ids = Map.singleton myId []
      in pure {inlines, ids}

    Conflict _ -> unsafeThrow "invariant violation: contentFromSyntax (Conflict _)"


-- TODO get anchor/focus vs start/end right
type RawSelection =
  { anchorKey :: String
  , anchorOffset :: Int
  , focusKey :: String
  , focusOffset :: Int

  -- , isBackward: Boolean
  -- , hasFocus: Boolean
  }

-- generic deriving fails on `WrappedRawSelection RawSelection`. Inline the
-- record instead.
newtype WrappedRawSelection = WrappedRawSelection -- RawSelection
  { anchorKey :: String
  , anchorOffset :: Int
  , focusKey :: String
  , focusOffset :: Int
  }

derive instance genericWrappedRawSelection :: Generic WrappedRawSelection
instance foreignWrappedRawSelection :: IsForeign WrappedRawSelection where
  read = readGeneric myOptions


newtype RawSelectSyntax a b = RawSelectSyntax
  { anchor :: Int
  , focus :: Int
  , syntax :: Syntax a b
  }

-- derive instance genericRawSelectSyntax :: Generic RawSelectSyntax
-- instance foreignRawSelectSyntax :: IsForeign RawSelectSyntax where
--   read = readGeneric myOptions

instance rawSelectSyntaxIsForeign :: (IsForeign a, IsForeign b) => IsForeign (RawSelectSyntax a b) where
  read obj = do
    anchor <- readProp "anchor" obj
    focus <- readProp "focus" obj
    syntax <- readProp "syntax" obj
    pure (RawSelectSyntax
      { anchor: anchor
      , focus: focus
      , syntax: syntax
      }
      )

-- | unrawSelectSyntax is used to initialize the editor.
-- |
-- | It takes a `RawSelectSyntax` -- a regular js object -- and turns it in to
-- | our internal representation.
unrawSelectSyntax
  :: forall a b. (Lang a b)
  => RawSelectSyntax a b
  -> Either String (ZoomedSZ a b)
unrawSelectSyntax (RawSelectSyntax {anchor: aOffset, focus: fOffset, syntax}) = do
  anchor <- makePath syntax aOffset
  focus <- makePath syntax fOffset
  pure (zoomIn {syntax, past: List.Nil, anchor, focus})

-- TODO test case where incoming map is empty
toPreEntityMap :: Map Int String -> PreEntityMap
toPreEntityMap entityTypes =
  let len = Map.size entityTypes
      lookups = flip map (0 .. (len - 1)) \ix -> Map.lookup ix entityTypes
  in PreEntityMap (sequence lookups)
