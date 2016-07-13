module Main where
-- Goal: complete roundtrip `Syntax -> RawSelection -> Action -> RawDraftContentBlock`

import Prelude (otherwise, (/), (<), (>=), (+), (-), (*), (<>), (==), (&&), pure, (<*>), (<$>), (<=), bind, show, class Eq)

import Control.Monad.State (State, modify, get, evalState)
import Data.Array ((:), {-uncons, reverse,-} concat, snoc)
import Data.Either (Either(..))
import Data.Foldable ({-foldr,-} foldl)
import Data.Foreign (Foreign, {-F,-} ForeignError(JSONError), toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Function.Uncurried (mkFn3, Fn3)
import Data.Int as I
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length)
-- import Data.Semigroup (class Semigroup)
import Data.Tuple (Tuple(Tuple), fst)

data PathStep = StepLeft | StepRight

instance pathStepIsEq :: Eq PathStep where
  eq StepLeft StepLeft = true
  eq StepRight StepRight = true
  eq _ _ = false

data Path
  = PathOffset Int
  | PathCons PathStep Path


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

data Syntax
  = SNumber Int
  | Plus Syntax Syntax
  | Hole String

instance syntaxIsForeign :: IsForeign Syntax where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "number" -> SNumber <$> readProp "value" obj
      "plus" -> Plus <$> readProp "l" obj <*> readProp "r" obj
      "hole" -> Hole <$> readProp "name" obj
      _ -> Left (JSONError "found unexpected value in syntaxIsForeign")

isDigit :: Char -> Boolean
isDigit x = x >= '0' && x <= '9'

cToI :: Char -> Int
cToI c = case I.fromString (show c) of
  Just i -> i
  Nothing -> 0

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
  { key :: Maybe BlockKey
    -- type :: 'unstyled'
  , text :: String
  , entityRanges :: Array EntityRange
  }

type BFromCState =
  { currentOffset :: Int
  -- XXX something more descriptive than String?
  , anchorKey :: String
  , anchorOffset :: Int
  , focusKey :: String
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

blockKey :: String
blockKey = "editor-block-key"

blockFromContent :: Array LightInline -> ContentState
blockFromContent inlines =
  let initialState =
        { currentOffset: 0
        , text: ""
        , entityRanges: []
        , anchorKey: blockKey
        , anchorOffset: 0
        , focusKey: blockKey
        , focusOffset: 0
        }

      accum :: BFromCState -> LightInline -> BFromCState
      accum state (InlinePlus i str info) = accumText i str (accumAnchorFocus info state)
      accum state (InlineNumber i str info) = accumText i str (accumAnchorFocus info state)
      accum state (InlineHole i str info) = accumText i str (accumAnchorFocus info state)

      -- For each LightInline, add its text to the current state
      accumText :: Int -> String -> BFromCState -> BFromCState
      accumText key str state =
        let tLen = length state.text
            sLen = length str
        in state
             { currentOffset = state.currentOffset + sLen
             , text = state.text <> str
             , entityRanges = snoc state.entityRanges
               { offset: tLen
               , length: sLen
               , key: key
               }
             }

      accumAnchorFocus :: InlineInfo -> BFromCState -> BFromCState
      accumAnchorFocus info state =
        -- TODO use state monad:
        -- whenJust info.anchor $ \i ->
        --   anchorOffset .=
        --   anchorKey .=
        let state' = case info.anchor of
              Just i -> state
                { anchorOffset = state.currentOffset + i
                , anchorKey = blockKey
                }
              Nothing -> state
            state'' = case info.focus of
              Just i -> state'
                { focusOffset = state'.currentOffset + i
                , focusKey = blockKey
                }
              Nothing -> state'
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

    SNumber n ->
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


operate :: Syntax -> Selection -> Action -> Either String Syntax
operate syntax selection action = case selection of
  AtomicSelection path -> operateAtomic syntax path action
  SpanningSelection p1 p2 -> Left "spanning actions not yet implemented"

-- need to make all these aware of location of cursor
operateAtomic :: Syntax -> Path -> Action -> Either String Syntax
operateAtomic (Plus l r) path action = case path of
  PathCons StepLeft rest ->
    Plus <$> operateAtomic l rest action <*> pure r
  PathCons StepRight rest ->
    Plus <$> pure l <*> operateAtomic r rest action
  -- TODO maybe we need to also handle parens?
  _ -> Left "ran out of steps when operating on Plus"

operateAtomic (Hole name) (PathOffset o) (Typing char)
  | name == "" && isDigit char = Right (SNumber (cToI char))
  | name == "" && char == '(' = Right (Plus (Hole "l") (Hole "r"))
  | otherwise = Right (Hole (name <> show char))
operateAtomic (SNumber n) (PathOffset o) (Typing char)
  | isDigit char = Right (SNumber (n * 10 + cToI char))
operateAtomic (SNumber n) (PathOffset o) Backspace
  | n >= 10 = Right (SNumber (n / 10))
  | otherwise = Right (Hole "")

operateAtomic _ _ _ = Left "had steps remaining at a leaf"

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

newtype WrappedRawSelection = WrappedRawSelection RawSelection

instance rawSelectionIsForeign :: IsForeign WrappedRawSelection where
  read obj = do
    anchorKey <- readProp "anchorKey" obj
    anchorOffset <- readProp "anchorOffset" obj
    focusKey <- readProp "focusKey" obj
    focusOffset <- readProp "focusOffset" obj
    pure (WrappedRawSelection
      { anchorKey: anchorKey
      , anchorOffset: anchorOffset
      , focusKey: focusKey
      , focusOffset: focusOffset
      }
      )

data Selection
  = SpanningSelection Path Path
  | AtomicSelection Path

type ContentState =
  { block :: RawDraftContentBlock
  -- TODO understand selectionBefore / selectionAfter
  , selection :: RawSelection
  }

rawSelectionToSelection :: RawSelection -> Syntax -> Either String Selection
rawSelectionToSelection rawSelection syntax =
  if rawSelection.anchorOffset == rawSelection.focusOffset
  then AtomicSelection <$> makePathMaybe syntax rawSelection.anchorOffset
  else SpanningSelection
         <$> makePathMaybe syntax rawSelection.anchorOffset
         <*> makePathMaybe syntax rawSelection.focusOffset

    -- XXX horrible naming please fix
    where makePathMaybe :: Syntax -> Int -> Either String Path
          makePathMaybe syntax n = case makePath syntax n of
            Left path -> Right path
            Right c -> Left
              ("rawSelectionToSelection overflow: " <> show n <> " demanded, "
              <> show c <> " consumed")

          -- keep moving in to the outermost syntax holding this offset
          -- either give back the resulting path or the number of chars
          -- consumed
          makePath :: Syntax -> Int -> Either Path Int
          makePath (SNumber n) offset =
            if offset < length (show n)
            then Left (PathOffset offset)
            else Right offset

          -- pretty much identical
          makePath (Hole name) offset =
            if offset < length name
            then Left (PathOffset offset)
            else Right offset

          -- [left] + [right]
          makePath (Plus left right) offset = case makePath left offset of
            Left path -> Left path
            Right consumed ->
              let offset' = offset - consumed
              in if offset' < 3
                 then Left (PathOffset offset')
                 else ((consumed + 3) + _) <$> makePath right offset'

rawOperate :: Syntax
           -> RawSelection
           -> Action
           -> Either String ContentState
rawOperate syntax rawSelection action = do
  selection <- rawSelectionToSelection rawSelection syntax
  syntax' <- operate syntax selection action
  -- TODO can probably get this from rawSelectionToSelection
  let yieldsContent = case selection of
        SpanningSelection l r -> contentFromSyntax syntax' (Just l) (Just r)
        AtomicSelection x -> contentFromSyntax syntax' (Just x) (Just x)
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (fst contentAndKeymapping)
  pure contentState

-- TODO change this from "operate" to "rawOperate"
rawOperateForeign :: Foreign -> Foreign -> Foreign -> Foreign
rawOperateForeign syntax rawSelection action =
  let allRead = Tuple3 <$> read syntax <*> read rawSelection <*> read action

      result :: Either String ContentState
      result = case allRead of
                 Left err -> Left (show err)
                 Right (Tuple3 syntax' (WrappedRawSelection rawSelection') action') ->
                   rawOperate syntax' rawSelection' action'

      -- toObj :: Syntax -> Foreign
      -- toObj (SNumber i) = toForeign { tag: "number", value: i }
      -- toObj (Plus l r) = toForeign { tag: "plus", l: toObj l, r: toObj r }
      -- toObj (Hole name) = toForeign { tag: "hole", name: name }

  in case result of
       Left err -> toForeign err
       Right result' -> toForeign (result' :: ContentState)

operateJs :: Fn3 Foreign Foreign Foreign Foreign
operateJs = mkFn3 rawOperateForeign
