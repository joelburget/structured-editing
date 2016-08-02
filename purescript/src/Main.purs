module Main where
-- Goal: complete roundtrip `Syntax -> RawSelection -> Action -> (Syntax, ContentState)`

import Prelude

import Control.Monad.State (State, modify, get, evalState, execState)
import Data.Array ((:), concatMap, snoc, (..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Generic (Options, SumEncoding(..), defaultOptions, readGeneric)
import Data.Function.Uncurried (mkFn2, Fn2, mkFn1, Fn1)
import Data.Generic (class Generic)
import Data.List as List
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.Traversable (sequence)

import Operate as Operate
import Path (Path, PathStep, subPath, getOffset)
import Syntax (SUnit, ZoomedSZ, SyntaxZipper, Syntax(..), zoomIn, makePath, zipUp, syntaxHoles)
import Template
import Util.String


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

blockFromContent :: Array LightInline -> ContentState
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

      accum :: BFromCState -> LightInline -> BFromCState
      accum state {ty, key, content, info} = case ty of
        InlineInternal -> accumText key "internal" content (accumAnchorFocus info state)
        InlineLeaf -> accumText key "leaf" content (accumAnchorFocus info state)
        InlineHole -> accumText key "hole" content (accumAnchorFocus info state)
        InlineConflict -> accumText key "conflict" content (accumAnchorFocus info state)

      -- For each LightInline, add its text to the current state
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

contentFromSyntax :: forall a b. Show b
                  => (Syntax a b)
                  -> Maybe Path
                  -> Maybe Path
                  -> State Int {inlines :: Array LightInline, ids :: Map Int (Array PathStep)}
contentFromSyntax syntax anchor focus = do
  let anchorOffset = getOffset anchor
      focusOffset = getOffset focus
  myId <- get
  modify (_ + 1)
  case syntax of
    Internal _ children -> do
      -- TODO -- returning this way causes us to traverse the array twice later
      children' <- iForM children \i child -> do
        {inlines, ids: childIds} <- contentFromSyntax child (subPath i anchor) (subPath i focus)
        pure
          { inlines
          , ids: (i:_) <$> childIds
          }

      -- XXX should be able to actually fail here
      let preInlines = plusTemplate myId anchorOffset focusOffset (map _.inlines children')
      let inlines = case preInlines of
            Just x -> x
            Nothing -> []

      let ids = Map.insert myId [] (Map.unions (map _.ids children'))
      pure {inlines, ids}

    Leaf n ->
      let content = show n
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

    Conflict l r ->
      let inlines = [
          -- XXX
          {-- {ty: InlineConflict, key: myId, content: str, info: inlineSelection 0 (String.length str) anchorOffset focusOffset} --}
          {-- {ty: InlineConflict, key: myId, content: str, info: inlineSelection 0 (String.length str) anchorOffset focusOffset} --}
          ]
          ids = Map.singleton myId []
      in pure {inlines, ids}


-- type RawSelection = { anchor :: Path, focus :: Path }
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

myOptions :: Options
myOptions = defaultOptions
  { sumEncoding = TaggedObject { tagFieldName: "tag", contentsFieldName: "value" }
  , unwrapNewtypes = true
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

unrawSelectSyntax :: forall a b. Show b => RawSelectSyntax a b -> Either String (ZoomedSZ a b)
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

newtype PreEntityMap = PreEntityMap (Maybe (Array String))
instance showPEM :: Show PreEntityMap where
  show (PreEntityMap m) = show m


type ContentState =
  { block :: RawDraftContentBlock
  -- TODO understand selectionBefore / selectionAfter
  , selection :: RawSelection
  , preEntityMap :: PreEntityMap
  }

initSelectSyntax :: Fn1 Foreign (Either String (ZoomedSZ SUnit Int))
initSelectSyntax = mkFn1 \foreignSelectSyntax -> do
  raw <- lmap show (read foreignSelectSyntax)
  unrawSelectSyntax raw

genContentState :: Fn1 (SyntaxZipper SUnit Int) Foreign
genContentState = mkFn1 \zipper ->
  let top = zipUp zipper
      -- TODO why the Justs?
      yieldsContent = contentFromSyntax top.syntax (Just top.anchor) (Just top.focus)
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (contentAndKeymapping.inlines)
  in toForeign contentState

operate :: Fn2 (SyntaxZipper SUnit Int) Foreign (Either String (SyntaxZipper SUnit Int))
operate = mkFn2 \zipper foreignAction -> do
  action <- lmap show (read foreignAction)
  Operate.operate zipper action

-- wrap this record so we can read it in `setEndpoints`
newtype WrappedAnchorFocus = WrappedAnchorFocus {anchor :: Int, focus :: Int}

derive instance genericWrappedAnchorFocus :: Generic WrappedAnchorFocus
instance foreignWrappedAnchorFocus :: IsForeign WrappedAnchorFocus where
  read = readGeneric myOptions

setEndpoints :: Fn2 (SyntaxZipper SUnit Int) Foreign (Either String (ZoomedSZ SUnit Int))
setEndpoints = mkFn2 \zipper foreignEndpoints -> do
  let top = zipUp zipper
  WrappedAnchorFocus {anchor: aOffset, focus: fOffset}
    <- lmap show (read foreignEndpoints)
  anchor <- makePath top.syntax aOffset
  focus <- makePath top.syntax fOffset
  let zipper' = top {anchor = anchor, focus = focus}
  pure (zoomIn zipper')

listLocalHoles :: Fn1 (SyntaxZipper SUnit Int) (Array String)
listLocalHoles = mkFn1 (_.syntax >>> syntaxHoles)

listAllHoles :: Fn1 (SyntaxZipper SUnit Int) (Array String)
listAllHoles = mkFn1 (zipUp >>> _.syntax >>> syntaxHoles)
