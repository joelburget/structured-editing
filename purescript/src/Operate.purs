module Operate where

import Prelude

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.Int as I
import Data.List ((:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length)
import Data.String as String
import Data.Map as Map
import Data.Map (Map, member)
import Data.Tuple (Tuple(..))

import Path (Path(..), (.+), PathStep, pathsDifferOnlyInOffset)
import Syntax (SyntaxZipper, Syntax(..), Past, up, down, getLeafTemplate, infer, updateChildType, constrainType, zoomIn, ZoomedSZ(..), makePath)
import Util (isDigit, spliceStr, spliceArr)
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

-- TODO this should be part of the language definition
operate :: LangZipper -> Action -> Either String LangZipper
operate zipper@{syntax, anchor, focus} action = if anchor == focus
  then operateAtomic zipper action
  else case Tuple anchor focus of
         -- kind of a hack to see if consuming `finish + 1` chars takes you
         -- beyond this syntax
         Tuple (PathOffset 0) (PathOffset finish) ->
           case makePath syntax (finish + 1) of
             -- left indicates failure to consume that many characters -- good!
             Left _ -> operateWithEntireNodeSelected zipper action
             -- right indicates success -- bad!
             Right _ -> Left "spanning actions not yet implemented"

         _ -> Left "spanning actions not yet implemented"

-- throwUserMessage :: forall a. String -> Operate a
throwUserMessage :: String -> Either String LangZipper
throwUserMessage = Left

-- | Propose an updated value at this point in the zipper
-- |
-- | Why do you need to "propose"? Because the update could conflict with other
-- | values.
-- |
-- | Uses updateChildType to move up the zipper and constrainType to move down.
propose :: LangSyntax -> LangZipper -> LangZipper
propose syntax z =
  let inferredTy = infer syntax
      oldTy = infer z.syntax
  -- as long as the type changes, keep marching up.
  -- TODO I'm not sure this check is even necessary
  in if oldTy == inferredTy
     then z {syntax = syntax}
     -- okay, we have differing types:
     -- * try to push this type up the tree with `updateChildType`
     else case up z of
       Just {zipper: z', prevLoc} -> case updateChildType z'.syntax {ix: prevLoc, newTm: syntax, newTy: inferredTy} of
         c@(Conflict _) ->
           let result = zoomIn $
                 z' { syntax = c
                    -- point to the term
                    , anchor = PathCons 0 (PathOffset 0)
                    , focus = PathCons 0 (PathOffset 0)
                    }
           in case result of ZoomedSZ result' -> result'
         syntax' -> propose syntax' z'
       -- reached the top, fill it in
       Nothing -> z {syntax = syntax}

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
