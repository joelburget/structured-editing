module Lang where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(JSONError), readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.Maybe (Maybe(..))
import Data.Tuple
import Syntax
import Template
import Path


data IntBoolLang = IntBoolLang

-- zipper and zoomed zipper for this particular language
type LangSyntax = Syntax Internal Leaf
type LangZipper = SyntaxZipper Internal Leaf
type ZoomedLang = ZoomedSZ Internal Leaf
type LangPast = Past Internal Leaf

data Internal
  = IfThenElse
  | Addition
  | Parens
  | Eq
  | ArrTy

derive instance genericInternal :: Generic Internal
instance showInternal :: Show Internal where show = gShow
instance eqInternal :: Eq Internal where eq = gEq
instance internalIsForeign :: IsForeign Internal where
  read obj = do
    tag <- readString obj
    case tag of
      "ifthenelse" -> pure IfThenElse
      "addition" -> pure Addition
      "parens" -> pure Parens
      "eq" -> pure Eq
      "arr-ty" -> pure ArrTy
      _ -> Left (JSONError "found unexpected value in internalIsForeign")

data Leaf
  = BoolLeaf Boolean
  | IntLeaf Int
  | IntTy
  | BoolTy
  | TyTy

derive instance genericLeaf :: Generic Leaf
instance showLeaf :: Show Leaf where show = gShow
instance eqLeaf :: Eq Leaf where eq = gEq

instance leafIsForeign :: IsForeign Leaf where
  read obj = do
    tag <- readProp "tag" obj
    case tag of
      "bool" -> BoolLeaf <$> readProp "value" obj
      "int" -> IntLeaf <$> readProp "value" obj
      "int-ty" -> pure IntTy
      "bool-ty" -> pure BoolTy
      "ty-ty" -> pure TyTy
      _ -> Left (JSONError "found unexpected value in leafIsForeign")


-- TODO this could be generalized to any language
-- type NormT = ExceptT (LangSyntax)
-- type Norm = NormT Identity

-- TODO Norm monad
norm :: LangSyntax -> LangSyntax
norm (Internal Addition [l, r]) =
  let l' = norm l
      r' = norm r
  in case Tuple l' r' of
       Tuple (Leaf (IntLeaf a)) (Leaf (IntLeaf b)) -> Leaf (IntLeaf (a + b))
       _ -> unsafeThrow "adding non-ints"
norm (Internal IfThenElse [i, l, r]) =
  let i' = norm i
      l' = norm l
      r' = norm r
  in case i' of
       Leaf (BoolLeaf i'') -> case Tuple l' r' of
         Tuple (Leaf (IntLeaf a)) (Leaf (IntLeaf b)) ->
           Leaf (IntLeaf (if i'' then a else b))
         Tuple (Leaf (BoolLeaf a)) (Leaf (BoolLeaf b)) ->
           Leaf (BoolLeaf (if i'' then a else b))
         _ -> unsafeThrow "non-matching if-then-else branches"
       _ -> unsafeThrow "if branching on non-boolean"
norm (Internal Parens [x]) = norm x
norm (Internal Eq [x, y]) =
  let x' = norm x
      y' = norm y
  in case Tuple x' y' of
       Tuple (Leaf (IntLeaf a)) (Leaf (IntLeaf b)) ->
         Leaf (BoolLeaf (a == b))
       Tuple (Leaf (BoolLeaf a)) (Leaf (BoolLeaf b)) ->
         Leaf (BoolLeaf (a == b))
       _ -> unsafeThrow "comparison type error"
norm l@(Leaf _) = l
norm h@(Hole _) = h
norm c@(Conflict _ _) = c
norm _ = unsafeThrow "evaluation error"

-- TODO there seems to always be a StepTy at the end, which could maybe be
-- implicit?
constrain :: LangSyntax -> Array (Constraint Internal Leaf)
constrain (Internal Addition _) =
  [ UnifyWith [StepChild 0, StepTy] (Leaf IntTy)
  , UnifyWith [StepChild 1, StepTy] (Leaf IntTy)
  , UnifyWith [StepTy] (Leaf IntTy)
  ]
constrain (Internal IfThenElse _) =
  [ UnifyWith [StepChild 0, StepTy] (Leaf BoolTy)
  , UnifyLocs [StepChild 0, StepTy] [StepChild 0, StepTy]
  ]
constrain (Internal Parens _) =
  [ UnifyLocs [StepChild 0, StepTy] [StepTy] ]
constrain (Internal Eq _) =
  [ UnifyLocs [StepChild 0, StepTy] [StepChild 1, StepTy] ]
constrain (Leaf (BoolLeaf _)) = [UnifyWith [StepTy] (Leaf BoolTy)]
constrain (Leaf (IntLeaf _))  = [UnifyWith [StepTy] (Leaf IntTy)]
constrain (Internal ArrTy _) = [UnifyWith [StepTy] (Leaf TyTy)]
constrain (Leaf IntTy)        = [UnifyWith [StepTy] (Leaf TyTy)]
constrain (Leaf BoolTy)       = [UnifyWith [StepTy] (Leaf TyTy)]
constrain (Leaf TyTy)         = [UnifyWith [StepTy] (Leaf TyTy)]
constrain (Hole _) = []
constrain (Conflict _ _) = []

instance intBoolIsLang :: Lang Internal Leaf where
  getLeafTemplate l = case l of
    Leaf (BoolLeaf b) -> show b
    Leaf (IntLeaf i) -> show i
    Leaf IntTy -> "int"
    Leaf BoolTy -> "bool"
    Leaf TyTy -> "type"
    _ -> unsafeThrow "inconsistency: couldn't get leaf template"

  getInternalTemplate i = case i of
    Internal tag _ -> mkTemplate $ case tag of
      IfThenElse -> "if {} then {} else {}"
      Addition -> "{} + {}"
      Parens -> "({})"
      Eq -> "{} == {}"
      ArrTy -> "{} -> {}"
    _ -> unsafeThrow "inconsistency: couldn't get internal template"

  normalize = norm

  constrainNeighbors = constrain
