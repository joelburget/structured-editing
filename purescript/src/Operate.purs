module Operate where

import Prelude

import Data.Either (Either(..))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.Int as I
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (length)
import Data.String as String

import Path (Path(..), (.+))
import Syntax (SyntaxZipper, Syntax(..))
import Util.String (isDigit, splice)

data Selection
  = SpanningSelection Path Path
  | AtomicSelection Path

derive instance genericSelection :: Generic Selection

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

operate :: SyntaxZipper -> Action -> Either String SyntaxZipper
operate zipper@{anchor, focus} action = if anchor == focus
  then operateAtomic zipper action
  else Left "spanning actions not yet implemented"

operateAtomic :: SyntaxZipper -> Action -> Either String SyntaxZipper
operateAtomic z@{syntax: Hole name, past, anchor: PathOffset o} (Typing char)
  | name == "" && isDigit char =
      case I.fromString (String.singleton char) of
        Just n -> Right
          { syntax: SyntaxNum n
          , past
          , anchor: z.anchor .+ 1
          , focus: z.anchor .+ 1
          }
        Nothing -> Left "insonsistency: unable to parse after inserting single digit"
  | name == "" && char == '(' = Right
      { syntax: Plus (Hole "l") (Hole "r")
      , past
      , anchor: z.anchor .+ 1
      , focus: z.anchor .+ 1
      }
  | otherwise = Right
      { syntax: Hole (splice name o 0 (String.singleton char))
      , past
      , anchor: z.anchor .+ 1
      , focus: z.anchor .+ 1
      }
operateAtomic z@{syntax: Hole name, past, anchor: PathOffset o} Backspace
  | name == "" = Left "backspacing out of empty hole"
  | o > length name
  = Left "inconsistency: backspacing with cursor past end of hole"
  | o == 0 = Left "backspacing out the left of a hole"
  | otherwise
  -- backspace goes left -- splice - 1!
  = let newName = splice name (o - 1) 1 ""
    in Right
         { syntax: Hole newName
         , past
         , anchor: z.anchor .+ (-1)
         , focus: z.anchor .+ (-1)
         }
operateAtomic z@{syntax: SyntaxNum n, past, anchor: PathOffset o} (Typing char)
  | char == '-' && o == 0
  = Right
      { syntax: SyntaxNum (-n)
      , past
      , anchor: z.anchor .+ 1
      , focus: z.anchor .+ 1
      }
  | isDigit char =
      case I.fromString (splice (show n) o 0 (String.singleton char)) of
        Just newNum -> Right
          { syntax: SyntaxNum newNum
          , past
          , anchor: z.anchor .+ 1
          , focus: z.anchor .+ 1
          }
        Nothing -> Left "inconsistency: unable to parse after inserting digit in number"
  | otherwise = Left "inserting non-digit in number"
operateAtomic z@{syntax: SyntaxNum n, past, anchor: PathOffset o} Backspace
  | o == 0 = Left "backspacing out the left of a number"
  | o > length (show n)
  = Left "inconsistency: backspacing with cursor past end of number"
  | n >= 10 || n < 0 = case I.fromString (splice (show n) (o - 1) 1 "") of
      -- backspace goes left -- splice - 1!
      Just newNum -> Right
        { syntax: SyntaxNum newNum
        , past
        , anchor: z.anchor .+ (-1)
        , focus: z.anchor .+ (-1)
        }
      Nothing -> Left "inconsistency: unable to parse after backspacing in number"
  | otherwise = Right
      { syntax: Hole ""
      , past
      , anchor: z.anchor .+ (-1)
      , focus: z.anchor .+ (-1)
      }

operateAtomic zipper action = Left $
  "had steps remaining at a leaf:\n\n" <>
  show zipper.syntax <> "\n\n" <>
  show zipper.anchor <> "\n\n" <>
  show action
