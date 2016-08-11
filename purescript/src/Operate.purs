module Operate where

import Prelude

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(..), isLeft)
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.Array as Array
import Data.Array.Partial (unsafeIndex)
import Data.Int as I
import Data.List ((:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length)
import Data.String as String
import Data.Map as Map
import Data.Map (Map, member)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Path (Path(..), (.+), PathStep)
import Syntax (class Lang, SyntaxZipper, Syntax(..), Past, up, down, getLeafTemplate, infer, propagateUpChildType, propagateDownChildTypes, zoomIn, ZoomedSZ(..), makePath)
import Util (isDigit, spliceStr, spliceArr, traceAnyId)
import Lang (LangZipper, Internal(..), Leaf(..), LangSyntax, LangPast)


data Action
  = Backspace
  | Typing Char

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
      _ -> Left (JSONError "found unexpected value in actionIsForeign")

leafKeywords :: Map String LangSyntax
leafKeywords = map Leaf $ Map.fromFoldable
  [ Tuple "true" (BoolLeaf true)
  , Tuple "false" (BoolLeaf false)
  , Tuple "type" TyTy
  , Tuple "int" IntTy
  , Tuple "bool" BoolTy
  ]

recognizeLeafKeyword :: String -> LangPast -> Path -> LangZipper
recognizeLeafKeyword name past anchor =
  let syntax = case Map.lookup name leafKeywords of
        Just s -> s
        Nothing -> unsafeThrow "inconsistency in recognizeLeafKeyword"
      -- TODO this is a small hack -- would be better to reuse the old zipper (and old zipper's hole)
      z = { syntax: Hole "to be filled by leaf keyword"
          , past
          , anchor: anchor .+ 1
          , focus: anchor .+ 1
          }
  in propose syntax z

internalKeywords :: Map String LangSyntax
internalKeywords = Map.fromFoldable
  [ Tuple "(" (Internal Parens [Hole "_"])
  , Tuple "+" (Internal Addition [Hole "_", Hole "_"])
  , Tuple "if" (Internal IfThenElse [Hole "_", Hole "_", Hole "_"])
  , Tuple "==" (Internal Eq [Hole "_", Hole "_"])
  , Tuple "->" (Internal ArrTy [Hole "_", Hole "_"])
  , Tuple ":" (Internal Annot [Hole "_", Hole "_"])
  ]

recognizeInternalKeyword :: String -> LangPast -> Path -> LangZipper
recognizeInternalKeyword name past anchor =
  let syntax = case Map.lookup name internalKeywords of
        Just s -> s
        Nothing -> unsafeThrow "inconsistency in recognizeInternalKeyword"
      start =
        { syntax
        , past
        , anchor: PathCons 0 (PathOffset 0)
        , focus: PathCons 0 (PathOffset 0)
        }
  in case down 0 start of
       Just z -> z
       Nothing -> unsafeThrow "inconsistency in recognizeInternalKeyword"

anchorIsAllLeft :: Path -> Boolean
anchorIsAllLeft (PathOffset 0) = true
anchorIsAllLeft (PathCons 0 rest) = anchorIsAllLeft rest
anchorIsAllLeft _ = false

focusIsAllRight :: forall a b. Lang a b => Path -> Syntax a b -> Boolean
-- we're looking for this to fail -- ie if we take one more step right we can't
-- consume enough characters, hence the `isLeft`
focusIsAllRight (PathOffset n) syntax = isLeft (makePath syntax (n + 1))
focusIsAllRight (PathCons dir rest) (Internal _ children) =
  let numChildren = Array.length children
      syntax' = unsafePartial (unsafeIndex children dir)
  in if dir == numChildren - 1
     then focusIsAllRight rest syntax'
     else false
focusIsAllRight _ _ = false

data SelectionSuggestions
  = MoveStart
  | MoveFinish
  | MoveBoth
  | NoSuggestion

-- | Suggest moving the current selection if it doesn't match one of the two
-- | cases:
-- | * it's entirely within a leaf
-- | * it perfectly wraps a node
-- |
-- | In other words, the selection must begin and end on the same level.
suggestCoherentSelection :: LangZipper -> SelectionSuggestions
suggestCoherentSelection zipper =
  let zipper' = zoomIn' zipper
      anchor = zipper'.anchor
      focus = zipper'.focus
  in if anchorIsAllLeft anchor && focusIsAllRight focus zipper'.syntax
     then NoSuggestion
     else case Tuple anchor focus of
            Tuple (PathOffset _) (PathOffset _) -> NoSuggestion
            Tuple (PathOffset _) (PathCons _ _) -> MoveStart
            Tuple (PathCons _ _) (PathOffset _) -> MoveFinish
            Tuple (PathCons _ _) (PathCons _ _) -> MoveBoth

-- TODO this should be part of the language definition
operate :: LangZipper -> Action -> Either String LangZipper
operate zipper@{syntax, anchor, focus} action = if anchor == focus
  then operateAtomic zipper action

  -- zoom in as far as possible, then:
  -- * the start must break out if you move it any further left
  --   (ie it must have offset 0)
  -- * the finish "..." right
  else let zipper' = zoomIn' zipper
           anchor = zipper'.anchor
           focus = zipper'.focus
       in if anchorIsAllLeft anchor && focusIsAllRight focus zipper'.syntax
          then operateWithEntireNodeSelected zipper action
          else case Tuple anchor focus of
                 Tuple (PathOffset aOff) (PathOffset fOff) ->
                   operateOffsetsInNode zipper action (min aOff fOff) (max aOff fOff)
                 _ -> Left "spanning actions not yet implemented (1)"

-- | Kind of cheating -- zoom and unwrap
zoomIn' :: forall a b. SyntaxZipper a b -> SyntaxZipper a b
zoomIn' z = case zoomIn z of ZoomedSZ z' -> z'

-- throwUserMessage :: forall a. String -> Operate a
throwUserMessage :: String -> Either String LangZipper
throwUserMessage = Left

-- | Propose an updated value at this point in the zipper
-- |
-- | Why do you need to "propose"? Because the update could conflict with other
-- | values.
-- |
-- | Uses propagateUpChildType to move up the zipper and constrainType to move down.
propose :: LangSyntax -> LangZipper -> LangZipper
propose syntax z =
  let inferredTy = infer syntax
      oldTy = infer z.syntax

      -- as long as the type changes, keep marching up.
      -- TODO I'm not sure this check is even necessary
      propagatedUp = traceAnyId $ if oldTy == inferredTy
        then z {syntax = syntax}
        -- okay, we have differing types:
        -- * try to push this type up the tree with `propagateUpChildType`
        else case up z of
          Just {zipper: z', prevLoc} ->
            let syntax' = propagateUpChildType
                  z'.syntax
                  {ix: prevLoc, newTm: syntax, newTy: inferredTy}
            in case syntax' of
                 c@(Conflict _) ->
                   z' { syntax = c
                      -- point to the term
                      , anchor = PathCons 0 (PathOffset 0)
                      , focus = PathCons 0 (PathOffset 0)
                      }
                 syntax' -> propose syntax' z'
          -- reached the top, fill it in
          Nothing -> z {syntax = syntax}

      -- now march down to update the children
      finalSyntax = traceAnyId $ propagateDownChildTypes
        { tm: propagatedUp.syntax
        -- XXX I *don't* think this is right
        , ty: inferredTy
        }
      propagatedDown = z {syntax = finalSyntax}

     -- make sure to zoom in once we've finished resolving changes
  in zoomIn' propagatedUp -- propagatedDown

operateOffsetsInNode :: LangZipper -> Action -> Int -> Int -> Either String LangZipper
operateOffsetsInNode zipper action start end = case Tuple zipper.syntax action of
  Tuple (Hole str) Backspace -> Right
    { syntax: Hole (spliceStr str start (end - start) "")
    , past: zipper.past
    , anchor: PathOffset start
    , focus: PathOffset start
    }
  Tuple (Hole str) (Typing char) -> Right
    { syntax: Hole (spliceStr str start (end - start) (String.singleton char))
    , past: zipper.past
    , anchor: PathOffset (start + 1)
    , focus: PathOffset (start + 1)
    }
  -- TODO other node types
  -- Tuple (Leaf (IntLeaf n)) Backspace ->
  _ -> Right zipper

operateWithEntireNodeSelected :: LangZipper -> Action -> Either String LangZipper
operateWithEntireNodeSelected zipper action =
  -- TODO this is a complete hack. backspacing will remove this character.
  let str = case action of
              Backspace -> "x"
              _ -> ""
      newZipper = { syntax: Hole str
                  , past: zipper.past
                  , anchor: PathOffset (length str)
                  , focus: PathOffset (length str)
                  }
  in operateAtomic newZipper action

-- filling a hole is easy
operateAtomic :: LangZipper -> Action -> Either String LangZipper
operateAtomic z@{syntax: Hole name, past, anchor: PathOffset o} (Typing char)
  | (name <> String.singleton char) `member` leafKeywords
  = Right $ recognizeLeafKeyword (name <> String.singleton char) past z.anchor
  | (name <> String.singleton char) `member` internalKeywords
  = Right $ recognizeInternalKeyword (name <> String.singleton char) past z.anchor
  | name == "" && isDigit char =
      case I.fromString (String.singleton char) of
        Just n -> Right $
          let z' = z {anchor = z.anchor .+ 1, focus = z.anchor .+ 1}
          in propose (Leaf (IntLeaf n)) z'
        Nothing -> throwUserMessage "inconsistency: unable to parse after inserting single digit"
  | otherwise = Right
      { syntax: Hole (spliceStr name o 0 (String.singleton char))
      , past
      , anchor: z.anchor .+ 1
      , focus: z.anchor .+ 1
      }

-- backspacing in a hole
operateAtomic z@{syntax: Hole name, past, anchor: PathOffset o} Backspace
  -- TODO this should really step up and backspace
  | name == "" = throwUserMessage "backspacing out of empty hole"
  | o > length name
  = throwUserMessage "inconsistency: backspacing with cursor past end of hole"
  | o == 0 = throwUserMessage "backspacing out the left of a hole"
  | otherwise
  -- backspace goes left -- splice - 1!
  = let newName = spliceStr name (o - 1) 1 ""
    in Right $
         if newName `member` leafKeywords
         then recognizeLeafKeyword newName past z.anchor
         else if newName `member` internalKeywords
              then recognizeInternalKeyword newName past z.anchor
              else { syntax: Hole newName
                   , past
                   , anchor: z.anchor .+ (-1)
                   , focus: z.anchor .+ (-1)
                   }

-- editing a number
operateAtomic z@{syntax: Leaf (IntLeaf n), past, anchor: PathOffset o} (Typing char)
  | char == '-' && o == 0
  = Right
      { syntax: Leaf (IntLeaf (-n))
      , past
      , anchor: z.anchor .+ 1
      , focus: z.anchor .+ 1
      }
  | isDigit char =
      case I.fromString (spliceStr (show n) o 0 (String.singleton char)) of
        Just newNum -> Right
          { syntax: Leaf (IntLeaf newNum)
          , past
          , anchor: z.anchor .+ 1
          , focus: z.anchor .+ 1
          }
        Nothing -> throwUserMessage "inconsistency: unable to parse after inserting digit in number (this is almost certainly because the number is larger than 32 bit int allows)"
  | otherwise = throwUserMessage "inserting non-digit in number"
operateAtomic z@{syntax: Leaf (IntLeaf n), past, anchor: PathOffset o} Backspace
  | o == 0 = throwUserMessage "backspacing out the left of a number"
  | o > length (show n)
  = throwUserMessage "inconsistency: backspacing with cursor past end of number"
  | n >= 10 || n < 0 = case I.fromString (spliceStr (show n) (o - 1) 1 "") of
      -- backspace goes left -- splice - 1!
      Just newNum -> Right
        { syntax: Leaf (IntLeaf newNum)
        , past
        , anchor: z.anchor .+ (-1)
        , focus: z.anchor .+ (-1)
        }
      Nothing -> throwUserMessage "inconsistency: unable to parse after backspacing in number"
-- TODO it should be easy for this to also swallow the above defn! Just recheck
-- whether we can parse the leaf.
operateAtomic z@{syntax: l@(Leaf _), past, anchor: PathOffset o} Backspace
  | o == 0 = throwUserMessage "backspacing out the left of a number"
  | o > length (getLeafTemplate l)
  = throwUserMessage "inconsistency: backspacing with cursor past end of leaf"
  | otherwise = Right
      { syntax: Hole (spliceStr (getLeafTemplate l) (o - 1) 1 "")
      , past
      , anchor: z.anchor .+ (-1)
      , focus: z.anchor .+ (-1)
      }

operateAtomic zipper action = throwUserMessage $
  "had steps remaining at a leaf:\n\n" <>
  show zipper.syntax <> "\n\n" <>
  show zipper.anchor <> "\n\n" <>
  show action
