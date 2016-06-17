module Main where
-- Goal: complete roundtrip `Syntax -> RawSelection -> Action -> RawDraftContentBlock`

import Prelude (otherwise, (/), (>=), (+), (-), (*), (++), (==), (&&), pure, (<*>), (<$>), (<=), bind, show, class Eq)

import Data.Array (uncons, concat, reverse, snoc)
import Data.Char as C
import Data.Either
import Data.Foldable (foldr, foldl)
import Data.Foreign (Foreign, F, ForeignError(JSONError), toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Function (mkFn3, Fn3)
import Data.Int as I
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))

data PathStep = StepLeft | StepRight

instance pathStepIsEq :: Eq PathStep where
  eq StepLeft StepLeft = true
  eq StepRight StepRight = true
  eq _ _ = false

data Path
  = PathOffset Int
  | PathCons PathStep Path


data Syntax
  = SNumber Int
  | Plus Syntax Syntax
  | Hole String

toObj :: Syntax -> Foreign
toObj (SNumber i) = toForeign { tag: "number", value: i }
toObj (Plus l r) = toForeign { tag: "plus", l: toObj l, r: toObj r }
toObj (Hole name) = toForeign { tag: "hole", name: name }

data Action
  = Backspace
  | Typing Char

instance actionIsForeign :: IsForeign Action where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "typing" -> Typing <$> readProp "value" obj
      "backspace" -> pure Backspace
      _ -> Left (JSONError "found unexpected value in actionIsForeign")

isDigit :: Char -> Boolean
isDigit x = x >= '0' && x <= '9'

cToI :: Char -> Int
cToI c = case I.fromString (C.toString c) of
  Just i -> i
  Nothing -> 0

type EntityRange =
  { offset :: Int
  , length :: Int
  , key :: String
  }

data LightInline
  = InlinePlus String InlineInfo
  | InlineNumber String InlineInfo
  | InlineHole String InlineInfo

type InlineInfo = { anchor :: Maybe Int , focus :: Maybe Int }

newtype BlockKey = BlockKey String
newtype EntityKey = EntityKey String

type RawDraftContentBlock =
  { key :: Maybe BlockKey
    -- type :: 'unstyled'
  , text :: String
  , entityRanges :: Array EntityRange
  }

type BFromCState =
  { currentOffset :: Int
  -- XXX something more descriptive than String?
  , anchorKey :: Maybe String
  , anchorOffset :: Int
  , focusKey :: Maybe String
  , focusOffset :: Int
  , text :: String
  , entityRanges :: Array EntityRange
  }

-- type AnchorFocusInfo =
--   { anchorKey :: Maybe String
--   , anchorOffset :: Int
--   , focusKey :: Maybe String
--   , focusOffset :: Int
--   }
--
-- type TextInfo =
--   { currentOffset :: Int
--   , text :: String
--   , entityRanges :: Array EntityRange
--   }

blockFromContent :: Array LightInline -> ContentState
blockFromContent inlines =
  let initialState =
        { currentOffset: 0
        , text: ""
        , entityRanges: []
        , anchorKey: Nothing
        , anchorOffset: 0
        , focusKey: Nothing
        , focusOffset: 0
        }

      accum :: BFromCState -> LightInline -> BFromCState
      accum state (InlinePlus str info) = accumText str (accumAnchorFocus info)
      accum state (InlineNumber str info) = accumText str (accumAnchorFocus info)
      accum state (InlineHole str info) = accumText str (accumAnchorFocus info)

      -- For each LightInline, add its text to the current state
      accumText :: String -> BFromCState -> BFromCState
      accumText str state =
        let tLen = length state.text
            sLen = length str
        in state
             { currentOffset = state.currentOffset + sLen
             , text = state.text + str
             , entityRanges = snoc state.entityRanges
               { offset: tLen
               , length: sLen
               , key: state.tag -- XXX
               }
             }

      accumAnchorFocus :: BlockKey -> InlineInfo -> BFromCState -> BFromCState
      accumAnchorFocus key info state =
        let state' = case info.anchor of
              Just i -> info
                { anchorOffset = state.currentOffset + i
                , anchorKey = key
                }
              Nothing -> info
            state'' = case info.focus of
              Just i -> info
                { focusOffset = state'.currentOffset + i
                , focusKey = key
                }
              Nothing -> info
        in state''

      finalState = foldl accum initialState inlines
  in { block: { key: Nothing
              , text: finalState.text
              , entityRanges: finalState.entityRanges
              }
     , selection: { anchorKey: finalState.anchorKey
                  , anchorOffset: finalState.anchorOffset
                  , focusKey: finalState.focusKey
                  , focusOffset: finalState.focusOffset
                  }
     }

getOffset :: Maybe Path -> Maybe Int
getOffset path = case path of
  Just (PathOffset n) -> Just n
  _ -> Nothing

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

subPath :: PathStep -> Maybe Path -> Maybe Path
subPath step path = case path of
  Nothing -> Nothing
  Just (PathOffset _) -> Nothing
  Just (PathCons step' rest) ->
    if step == step'
       then Just rest
       else Nothing

contentFromSyntax :: Syntax -> Maybe Path -> Maybe Path -> Array LightInline
contentFromSyntax syntax anchor focus =
  let anchorOffset = getOffset anchor
      focusOffset = getOffset focus
  in case syntax of
        -- XXX need to flatten?
        Plus l r -> concat
          [ [InlinePlus "(" (inlineSelection 0 1 anchorOffset focusOffset)]
          , contentFromSyntax
              l
              (subPath StepLeft anchor)
              (subPath StepLeft focus)
          , [InlinePlus " + " (inlineSelection 1 3 anchorOffset focusOffset)]
          , contentFromSyntax
              r
              (subPath StepRight anchor)
              (subPath StepRight focus)
          , [InlinePlus ")" (inlineSelection 4 1 anchorOffset focusOffset)]
          ]
        SNumber n ->
          let text = show n
          in [
               InlineNumber text (inlineSelection 0 (length text) anchorOffset focusOffset)
             ]
        Hole str ->
          [
            InlineHole str (inlineSelection 0 (length str) anchorOffset focusOffset)
          ]


-- need to make all these aware of location of cursor
operate :: Syntax -> Path -> Action -> Either String Syntax
operate (Plus l r) path action = case path of
  PathCons StepLeft rest ->
    Plus <$> operate l rest action <*> pure r
  PathCons StepRight rest ->
    Plus <$> pure l <*> operate r rest action
  -- TODO maybe we need to also handle parens?
  _ -> Left "ran out of steps when operating on Plus"

operate (Hole name) (PathOffset o) (Typing char)
  | name == "" && isDigit char = Right (SNumber (cToI char))
  | name == "" && char == '(' = Right (Plus (Hole "l") (Hole "r"))
  | otherwise = Right (Hole (name ++ C.toString char))
operate (SNumber n) (PathOffset o) (Typing char)
  | isDigit char = Right (SNumber (n * 10 + cToI char))
operate (SNumber n) (PathOffset o) Backspace
  | n >= 10 = Right (SNumber (n / 10))
  | otherwise = Right (Hole "")

operate _ _ _ = Left "had steps remaining at a leaf"

data Tuple3 a b c = Tuple3 a b c

-- type RawSelection = { anchor :: Path, focus :: Path }
type RawSelection =
  { anchorKey :: String
  , anchorOffset :: Int
  , focusKey :: String
  , focusOffset :: Int

  -- , isBackward: Boolean
  -- , hasFocus: Boolean
  }

data Selection
  = SpanningSelection Path Path
  | AtomicSelection Path

type ContentState =
  { block :: RawDraftContentBlock
  -- TODO understand selectionBefore / selectionAfter
  , selection :: RawSelection
  }

rawSelectionToSelection :: RawSelection -> Selection
rawSelectionToSelection rawSelection =
  if rawSelection.anchorOffset == rawSelection.focusOffset
  then AtomicSelection (makePath rawSelection.anchorOffset)
  else SpanningSelection (makePath rawSelection.anchorOffset) (makePath rawSelection.focusOffset)
    where makePath = undefined

rawOperate :: Syntax
           -> RawSelection
           -> Action
           -> Either String ContentState
rawOperate syntax rawSelection action =
  let selection = rawSelectionToSelection rawSelection
      syntax' = operate syntax selection action
      -- TODO can probably get this from rawSelectionToSelection
      content = case selection of
        SpanningSelection l r -> contentFromSyntax syntax' l r
        AtomicSelection x -> contentFromSyntax syntax' x x
      contentState = blockFromContent content
  in contentState

-- TODO change this from "operate" to "rawOperate"
rawOperateForeign :: Foreign -> Foreign -> Foreign -> Foreign
rawOperateForeign contentState action =
  let allRead = Tuple3 <$> read syntax <*> read rawSelection <*> read action
      result :: Either String Syntax
      result = case allRead of
                 Left err -> Left (show err)
                 Right (Tuple3 syntax' path' action') -> rawOperate syntax' rawSelection' action'
  in case result of
       Left err -> toForeign err
       Right result' -> toObj (result' :: Syntax)

operateJs :: Fn3 Foreign Foreign Foreign Foreign
operateJs = mkFn3 rawOperateForeign
