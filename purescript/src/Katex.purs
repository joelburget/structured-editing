module Katex where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(JSONError), readString)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Function.Uncurried (mkFn1, Fn1)

import Interface as Interface
import Path
import Syntax
import Template

data Internal
  = Sqrt
  | Overline

instance internalIsForeign :: IsForeign Internal where
  read obj = do
    tag <- readString obj
    case tag of
      "sqrt" -> pure Sqrt
      "overline" -> pure Overline
      _ -> Left (JSONError "found unexpected value in internalIsForeign")

functions =
  [ {name: "sqrt", numArgs: 1, numOptArgs: 1}
  , {name: "overline", numArgs: 1, numOptArgs: 1}
  ]

data Leaf
  = Equiv
  | Prec
  | Succ
  | Str String

instance leafIsForeign :: IsForeign Leaf where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "equiv" -> pure Equiv
      "prec" -> pure Prec
      "succ" -> pure Succ
      "str" -> Str <$> readProp "value" obj
      _ -> Left (JSONError "found unexpected value in leafIsForeign")

symbols =
  [ "equiv"
  , "prec"
  , "succ"
  ]

instance katexIsTemplated :: TemplatedTree Internal Leaf where
  getLeafTemplate = case _ of
    Leaf Equiv -> "equiv"
    Leaf Prec -> "prec"
    Leaf Succ -> "succ"
    Leaf (Str str) -> str
    _ -> unsafeThrow "inconsistency: couldn't get leaf template"

  getInternalTemplate = case _ of
    Internal tag _ -> mkTemplate $ case tag of
      -- TODO optional args!
      Sqrt -> "sqrt{{}}"
      Overline -> "overline{{}}"
    _ -> unsafeThrow "inconsistency: couldn't get internal template"

initSelectSyntax :: Fn1 Foreign (Either String KatexZoomed)
initSelectSyntax = Interface.initSelectSyntax

toOpaque :: Fn1 Foreign (Either String KatexSyntax)
toOpaque = mkFn1 (read >>> lmap show)

type KatexSyntax = Syntax Internal Leaf
type KatexZipper = SyntaxZipper Internal Leaf
type KatexZoomed = ZoomedSZ Internal Leaf
type KatexPast = Past Internal Leaf
type KatexConflictInfo = ConflictInfo Internal Leaf
