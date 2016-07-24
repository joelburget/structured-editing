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

import Path (Path(..), PathStep(..))
import Syntax (Syntax(..))
import Util.String (isDigit, splice)

data Selection
  = SpanningSelection Path Path
  | AtomicSelection Path

derive instance genericSelection :: Generic Selection

newtype SelectSyntax = SelectSyntax
  { selection :: Selection
  , syntax :: Syntax
  }

derive instance genericSelectSyntax :: Generic SelectSyntax
instance eqSelectSyntax :: Eq SelectSyntax where eq = gEq
instance showSelectSyntax :: Show SelectSyntax where show = gShow

data Action
  = Backspace
  | Typing Char

-- derive instance genericAction :: Generic Action

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

operate :: SelectSyntax -> Action -> Either String SelectSyntax
operate (SelectSyntax {selection, syntax}) action = case selection of
  AtomicSelection path -> operateAtomic syntax path action
  SpanningSelection p1 p2 -> Left "spanning actions not yet implemented"

-- need to make all these aware of location of cursor
operateAtomic :: Syntax -> Path -> Action -> Either String SelectSyntax
operateAtomic (Plus l r) path action = case path of
  PathCons StepLeft rest -> do
    SelectSyntax {selection, syntax} <- operateAtomic l rest action
    case selection of
      AtomicSelection path ->
        pure (SelectSyntax {
          selection: AtomicSelection (PathCons StepLeft path),
          syntax: Plus syntax r
          })
      SpanningSelection _ _ -> Left "can't yet handle spanning selection"
  PathCons StepRight rest -> do
    SelectSyntax {selection, syntax} <- operateAtomic r rest action
    case selection of
      AtomicSelection path ->
        pure (SelectSyntax {
          selection: AtomicSelection (PathCons StepRight path),
          syntax: Plus l syntax
          })
      SpanningSelection _ _ -> Left "can't yet handle spanning selection"
  -- TODO maybe we need to also handle parens?
  _ -> Left "ran out of steps when operating on Plus"

operateAtomic (Hole name) (PathOffset o) (Typing char)
  | name == "" && isDigit char =
      case I.fromString (String.singleton char) of
        Just n -> Right (SelectSyntax
          { syntax: SyntaxNum n
          , selection: AtomicSelection (PathOffset (o + 1))
          })
        Nothing -> Left "insonsistency: unable to parse after inserting single digit"
  | name == "" && char == '(' = Right (
      SelectSyntax {
        syntax: Plus (Hole "l") (Hole "r"),
        selection: AtomicSelection (PathOffset (o + 1))
      })
  | otherwise = Right (
      SelectSyntax {
        syntax: Hole (splice name o 0 (String.singleton char)),
        selection: AtomicSelection (PathOffset (o + 1))
      })
operateAtomic (Hole name) (PathOffset o) Backspace
  | name == "" = Left "backspacing out of empty hole"
  | o > length name
  = Left "inconsistency: backspacing with cursor past end of hole"
  | o == 0 = Left "backspacing out the left of a hole"
  | otherwise
  -- backspace goes left -- splice - 1!
  = let newName = splice name (o - 1) 1 ""
    in Right (
      SelectSyntax {
        syntax: Hole newName,
        selection: AtomicSelection (PathOffset (o - 1))
      })
operateAtomic (SyntaxNum n) (PathOffset o) (Typing char)
  | char == '-' && o == 0
  = Right (SelectSyntax
      { syntax: SyntaxNum (-n)
      , selection: AtomicSelection (PathOffset (o + 1))
      })
  | isDigit char =
      case I.fromString (splice (show n) o 0 (String.singleton char)) of
        Just newNum -> Right (SelectSyntax
          { syntax: SyntaxNum newNum
          , selection: AtomicSelection (PathOffset (o + 1))
          })
        Nothing -> Left "inconsistency: unable to parse after inserting digit in number"
  | otherwise = Left "inserting non-digit in number"
operateAtomic (SyntaxNum n) (PathOffset o) Backspace
  | o == 0 = Left "backspacing out the left of a number"
  | o > length (show n)
  = Left "inconsistency: backspacing with cursor past end of number"
  | n >= 10 || n < 0 = case I.fromString (splice (show n) (o - 1) 1 "") of
      -- backspace goes left -- splice - 1!
      Just newNum -> Right (SelectSyntax
        { syntax: SyntaxNum newNum
        , selection: AtomicSelection (PathOffset (o - 1))
        })
      Nothing -> Left "inconsistency: unable to parse after backspacing in number"
  | otherwise = Right (
      SelectSyntax {
        syntax: Hole "",
        selection: AtomicSelection (PathOffset (o - 1))
      })

operateAtomic _ _ _ = Left "had steps remaining at a leaf"
