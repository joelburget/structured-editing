module Main where
-- Goal: complete roundtrip `Syntax -> RawSelection -> Action -> (Syntax, ContentState)`

import Prelude

import Control.Monad.State (State, modify, get, evalState, execState)
import Data.Array ((:), concat, snoc, (..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Generic (Options, SumEncoding(..), defaultOptions, readGeneric)
import Data.Function.Uncurried (mkFn2, Fn2, mkFn1, Fn1)
import Data.Generic (class Generic)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), fst)

import Operate (Action, SelectSyntax(..), Selection(..), operate)
import Path (Path, PathStep(..), subPath, getOffset)
import Syntax (Syntax(..), mkForeign, makePath, unmakePath)
import Util.String (whenJust)


myOptions :: Options
myOptions = defaultOptions
  { sumEncoding = TaggedObject { tagFieldName: "tag", contentsFieldName: "value" }
  , unwrapNewtypes = true
  }


type EntityRange =
  { offset :: Int
  , length :: Int
  , key :: Int
  }

data LightInline
  = InlinePlus Int String InlineInfo
  | InlineNumber Int String InlineInfo
  | InlineHole Int String InlineInfo

type InlineInfo = { anchor :: Maybe Int , focus :: Maybe Int }

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
      accum state (InlinePlus i str info) = accumText i "plus" str (accumAnchorFocus info state)
      accum state (InlineNumber i str info) = accumText i "number" str (accumAnchorFocus info state)
      accum state (InlineHole i str info) = accumText i "hole" str (accumAnchorFocus info state)

      -- For each LightInline, add its text to the current state
      accumText :: Int -> String -> String -> BFromCState -> BFromCState
      accumText key entityType str state =
        let offset = length state.text
            sLen = length str
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

inlineSelection :: Int -> Int -> Maybe Int -> Maybe Int -> InlineInfo
inlineSelection start len anchorOffset focusOffset =
  let f offset = case offset of
        Just n -> if start <= n && n <= start + len
                     then Just (n - start)
                     else Nothing
        Nothing -> Nothing
  in { anchor: f anchorOffset
     , focus: f focusOffset
     }

contentFromSyntax :: Syntax
                  -> Maybe Path
                  -> Maybe Path
                  -> State Int (Tuple (Array LightInline) (Map Int (Array PathStep)))
contentFromSyntax syntax anchor focus = do
  let anchorOffset = getOffset anchor
      focusOffset = getOffset focus
  myId <- get
  modify (_+1)
  case syntax of
    -- XXX need to flatten?
    Plus l r -> do
      Tuple l' m1 <- contentFromSyntax
        l
        (subPath StepLeft anchor)
        (subPath StepLeft focus)
      Tuple r' m2 <- contentFromSyntax
        r
        (subPath StepRight anchor)
        (subPath StepRight focus)
      let arr = concat
            [ [InlinePlus myId "(" (inlineSelection 0 1 anchorOffset focusOffset)]
            , l'
            , [InlinePlus myId " + " (inlineSelection 1 3 anchorOffset focusOffset)]
            , r'
            , [InlinePlus myId ")" (inlineSelection 4 1 anchorOffset focusOffset)]
            ]

      let mergedMap = Map.insert
            myId []
            (Map.union ((StepLeft:_) <$> m1) ((StepRight:_) <$> m2))
      pure (Tuple arr mergedMap)

    SyntaxNum n ->
      let text = show n
          arr = [
            InlineNumber myId text (inlineSelection 0 (length text) anchorOffset focusOffset)
          ]
      in pure (Tuple arr (Map.singleton myId []))
    Hole str ->
      let arr = [
            InlineHole myId str (inlineSelection 0 (length str) anchorOffset focusOffset)
          ]
      in pure (Tuple arr (Map.singleton myId []))



newtype RawSelectSyntax = RawSelectSyntax
  { anchor :: Int
  , focus :: Int
  , syntax :: Syntax
  }

-- derive instance genericRawSelectSyntax :: Generic RawSelectSyntax
-- instance foreignRawSelectSyntax :: IsForeign RawSelectSyntax where
--   read = readGeneric myOptions

instance rawSelectSyntaxIsForeign :: IsForeign RawSelectSyntax where
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

derive instance genericWrappedRawSelection :: Generic WrappedRawSelection
instance foreignWrappedRawSelection :: IsForeign WrappedRawSelection where
  read = readGeneric myOptions

toPreEntityMap :: Map Int String -> PreEntityMap
toPreEntityMap entityTypes =
  let len = Map.size entityTypes
      lookups = flip map (0 .. (len - 1)) \ix -> Map.lookup ix entityTypes
  in PreEntityMap (sequence lookups)

newtype PreEntityMap = PreEntityMap (Maybe (Array String))


type ContentState =
  { block :: RawDraftContentBlock
  -- TODO understand selectionBefore / selectionAfter
  , selection :: RawSelection
  , preEntityMap :: PreEntityMap
  }

unrawSelectSyntax :: RawSelectSyntax -> Either String SelectSyntax
unrawSelectSyntax rss@(RawSelectSyntax {anchor, focus, syntax}) = do
  selection <- rawSelectSyntaxToSelection rss
  pure (SelectSyntax ({syntax, selection}))

makeRawSelectSyntax :: SelectSyntax -> Either String RawSelectSyntax
makeRawSelectSyntax (SelectSyntax {syntax, selection}) =
  case selection of
       -- TODO mixing up the notions of anchor/focus and left/right
    SpanningSelection lpath rpath -> do
      anchor <- unmakePath syntax lpath
      focus <- unmakePath syntax rpath
      pure (RawSelectSyntax {anchor, focus, syntax})
    AtomicSelection path -> do
      i <- unmakePath syntax path
      pure (RawSelectSyntax {anchor: i, focus: i, syntax})

rawSelectSyntaxToSelection :: RawSelectSyntax -> Either String Selection
rawSelectSyntaxToSelection (RawSelectSyntax {anchor, focus, syntax}) =
  let rawSelection =
        { anchorKey: ""
        , anchorOffset: anchor
        , focusKey: ""
        , focusOffset: focus
        }
  in rawSelectionToSelection rawSelection syntax

rawSelectionToSelection :: RawSelection -> Syntax -> Either String Selection
rawSelectionToSelection rawSelection syntax =
  if rawSelection.anchorOffset == rawSelection.focusOffset
  then AtomicSelection <$> makePath syntax rawSelection.anchorOffset
  else SpanningSelection
         <$> makePath syntax rawSelection.anchorOffset
         <*> makePath syntax rawSelection.focusOffset


contentStateFromSelectSyntax :: SelectSyntax -> Either String ContentState
contentStateFromSelectSyntax (SelectSyntax rec) = do
  -- TODO can probably get this from rawSelectionToSelection
  let yieldsContent = case rec.selection of
        SpanningSelection l r -> contentFromSyntax rec.syntax (Just l) (Just r)
        AtomicSelection x -> contentFromSyntax rec.syntax (Just x) (Just x)
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (fst contentAndKeymapping)
  pure contentState

contentStateFromSelectSyntaxJs :: Fn1 Foreign Foreign
contentStateFromSelectSyntaxJs =
  let eitherF :: Foreign -> Either String ContentState
      eitherF f = do
        rawSelectSyntax <- lmap show (read f)
        selectSyntax <- unrawSelectSyntax rawSelectSyntax
        contentStateFromSelectSyntax selectSyntax
      foreignF :: Foreign -> Foreign
      foreignF = either toForeign toForeign <<< eitherF
  in mkFn1 foreignF

operateJs :: Fn2 Foreign Foreign Foreign
operateJs = mkFn2 rawOperateForeign

rawOperateForeign :: Foreign -> Foreign -> Foreign
rawOperateForeign selectSyntax action =
  let allRead = Tuple <$> read selectSyntax <*> read action

      -- "raw" in the sense that it's operating on a raw selection
      rawOperate :: RawSelectSyntax
                 -> Action
                 -> Either String RawSelectSyntax
      rawOperate rawSelectSyntax action = do
        selectSyntax <- unrawSelectSyntax rawSelectSyntax
        ss <- operate selectSyntax action
        makeRawSelectSyntax ss

      result :: Either String RawSelectSyntax
      result = case allRead of
        Left err -> Left (show err)
        Right (Tuple rawSelectSyntax action') ->
          rawOperate rawSelectSyntax action'

  in case result of
       Left err -> toForeign err
       Right (RawSelectSyntax {anchor, focus, syntax}) ->
       -- take care to serialize in the object-ey, not classy way
         toForeign {anchor, focus, syntax: mkForeign syntax}
