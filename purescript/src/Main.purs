module Main where
-- Goal: complete roundtrip `Syntax -> RawSelection -> Action -> (Syntax, ContentState)`

import Prelude (otherwise, (/), (<), (>), (>=), (+), (-), (*), (<>), (==), (&&), pure, (<*>), (<$>), (<=), bind, show, class Eq, class Show, (<<<))

import Control.Monad.State (State, modify, get, evalState)
import Data.Array ((:), concat, snoc)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Foreign (Foreign, ForeignError(JSONError), toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Function.Uncurried (mkFn3, Fn3, mkFn2, Fn2, mkFn1, Fn1)
import Data.Int as I
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length, take, drop)
import Data.String as String
import Data.Tuple (Tuple(Tuple), fst)


data Tuple3 a b c = Tuple3 a b c

data PathStep = StepLeft | StepRight

instance pathStepIsEq :: Eq PathStep where
  eq StepLeft StepLeft = true
  eq StepRight StepRight = true
  eq _ _ = false

instance pathStepIsShow :: Show PathStep where
  show StepLeft = "StepLeft"
  show StepRight = "StepRight"

data Path
  = PathOffset Int
  | PathCons PathStep Path


instance pathIsShow :: Show Path where
  show (PathOffset i) = "PathOffset " <> show i
  show (PathCons s p) = "PathCons " <> show s <> " " <> show p


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
cToI c = case I.fromString (String.singleton c) of
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
  }

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
  in { block: { key: BlockKey blockKey
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
  | otherwise = Right (Hole (name <> String.singleton char))
operateAtomic (Hole name) (PathOffset o) Backspace
  | name == "" = Left "backspacing out of empty hole"
  | o > length name
  = Left "inconsistency: backspacing with cursor past end of hole"
  | o == 0 = Left "backspacking out the left of a hole"
  | otherwise
  = let newName = take (o - 1) name <> drop o name
    in Right (Hole newName)
operateAtomic (SNumber n) (PathOffset o) (Typing char)
  | isDigit char = Right (SNumber (n * 10 + cToI char))
operateAtomic (SNumber n) (PathOffset o) Backspace
  | n >= 10 = Right (SNumber (n / 10))
  | otherwise = Right (Hole "")

operateAtomic _ _ _ = Left "had steps remaining at a leaf"

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
  then AtomicSelection <$> makePath syntax rawSelection.anchorOffset
  else SpanningSelection
         <$> makePath syntax rawSelection.anchorOffset
         <*> makePath syntax rawSelection.focusOffset

    where makePath :: Syntax -> Int -> Either String Path
          makePath syntax n = lmap
            (\c -> "rawSelectionToSelection overflow: " <>
              show n <> " demanded, " <> show c <> " consumed")
            (consumePath syntax n)

          -- keep moving in to the outermost syntax holding this offset
          -- either give back the resulting path or the number of chars
          -- consumed
          consumePath :: Syntax -> Int -> Either Int Path
          consumePath (SNumber n) offset =
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
          consumePath (Plus left right) offset =
            case consumePath left (offset - 1) of
              Right path -> Right (PathCons StepLeft path)
              Left consumed ->
                let offset' = offset - (consumed + 1)
                in if offset' < 3
                   then Right (PathOffset offset')
                   else case consumePath right (offset' - 3) of
                          Right path -> Right (PathCons StepRight path)
                          Left consumed' -> Left (consumed' + consumed + 4)

rawOperateForeign :: Foreign -> Foreign -> Foreign -> Foreign
rawOperateForeign syntax rawSelection action =
  let allRead = Tuple3 <$> read syntax <*> read rawSelection <*> read action

      -- | "raw" in the sense that it's operating on a raw selection
      rawOperate :: Syntax
                 -> RawSelection
                 -> Action
                 -> Either String (Tuple Syntax ContentState)
      rawOperate syntax rawSelection action = do
        selection <- rawSelectionToSelection rawSelection syntax
        syntax' <- operate syntax selection action
        -- TODO can probably get this from rawSelectionToSelection
        let yieldsContent = case selection of
              SpanningSelection l r -> contentFromSyntax syntax' (Just l) (Just r)
              AtomicSelection x -> contentFromSyntax syntax' (Just x) (Just x)
            contentAndKeymapping = evalState yieldsContent 0
            contentState = blockFromContent (fst contentAndKeymapping)
        pure (Tuple syntax contentState)

      result :: Either String (Tuple Syntax ContentState)
      result = case allRead of
        Left err -> Left (show err)
        Right (Tuple3 syntax' (WrappedRawSelection rawSelection') action') ->
          rawOperate syntax' rawSelection' action'

  in case result of
       Left err -> toForeign err
       Right result' -> toForeign result'

operateJs :: Fn3 Foreign Foreign Foreign Foreign
operateJs = mkFn3 rawOperateForeign

data SelectSyntax = SelectSyntax
  { anchor :: Int
  , focus :: Int
  , syntax :: Syntax
  }

instance selectSyntaxIsForeign :: IsForeign SelectSyntax where
  read obj = do
    anchor <- readProp "anchor" obj
    focus <- readProp "focus" obj
    syntax <- readProp "syntax" obj
    pure (SelectSyntax
      { anchor: anchor
      , focus: focus
      , syntax: syntax
      }
      )

contentStateFromSelectSyntax :: SelectSyntax -> Either String ContentState
contentStateFromSelectSyntax (SelectSyntax rec) = do
  let rawSelection =
        { anchorOffset: rec.anchor
        , anchorKey: ""
        , focusOffset: rec.focus
        , focusKey: ""
        }
  selection <- rawSelectionToSelection rawSelection rec.syntax
  -- TODO can probably get this from rawSelectionToSelection
  let yieldsContent = case selection of
        SpanningSelection l r -> contentFromSyntax rec.syntax (Just l) (Just r)
        AtomicSelection x -> contentFromSyntax rec.syntax (Just x) (Just x)
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (fst contentAndKeymapping)
  pure contentState

contentStateFromSelectSyntaxJs :: Fn1 Foreign Foreign
contentStateFromSelectSyntaxJs =
  let eitherF :: Foreign -> Either String ContentState
      eitherF f = do
        selectSyntax <- lmap show (read f)
        contentStateFromSelectSyntax selectSyntax
      foreignF :: Foreign -> Foreign
      foreignF = either toForeign toForeign <<< eitherF
  in mkFn1 foreignF

initForeign :: Foreign -> Foreign -> Foreign
initForeign syntax rawSelection =
  let allRead = Tuple <$> read syntax <*> read rawSelection

      rawInit :: Syntax
              -> RawSelection
              -> Either String ContentState
      rawInit syntax rawSelection = do
        selection <- rawSelectionToSelection rawSelection syntax
        let yieldsContent = case selection of
              SpanningSelection l r -> contentFromSyntax syntax (Just l) (Just r)
              AtomicSelection x -> contentFromSyntax syntax (Just x) (Just x)
            contentAndKeymapping = evalState yieldsContent 0
            contentState = blockFromContent (fst contentAndKeymapping)
        pure contentState

      result :: Either String ContentState
      result = case allRead of
        Left err -> Left (show err)
        Right (Tuple syntax' (WrappedRawSelection rawSelection')) ->
          rawInit syntax' rawSelection'
  in case result of
       Left err -> toForeign err
       Right result' -> toForeign result'

initJs :: Fn2 Foreign Foreign Foreign
initJs = mkFn2 initForeign
