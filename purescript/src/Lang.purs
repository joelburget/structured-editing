module Lang where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(..), isLeft, either)
import Data.Foldable (any)
import Data.Foreign (ForeignError(JSONError), readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple
import Syntax
import Template
import Path
import Util


-- zipper and zoomed zipper for this particular language
type LangSyntax = Syntax Internal Leaf
type LangZipper = SyntaxZipper Internal Leaf
type ZoomedLang = ZoomedSZ Internal Leaf
type LangPast = Past Internal Leaf
type LangConflictInfo = ConflictInfo Internal Leaf

data Internal
  = IfThenElse
  | Addition
  | Parens
  | Eq
  | ArrTy
  | Annot

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
      "annot" -> pure Annot
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

bool = Leaf BoolTy
int = Leaf IntTy
ty = Leaf TyTy


-- TODO this could be generalized to any language
-- type NormT = ExceptT (LangSyntax)
-- type Norm = NormT Identity

normalizeChildren :: Array LangSyntax -> Either (Array LangSyntax) (Array LangSyntax)
normalizeChildren arr =
  let normArr = map norm arr
      retArr = map (either id id) normArr
  in if any isLeft normArr then Left retArr else Right retArr

-- | Normalize as much as possible. Indicates non-full normalization with a `Left`
-- TODO Norm monad
norm :: LangSyntax -> Either LangSyntax LangSyntax
norm i@(Internal Addition [l, r]) = case normalizeChildren [l, r] of
  Right [Leaf (IntLeaf a), Leaf (IntLeaf b)] -> Right $ Leaf (IntLeaf (a + b))
  Right _ -> unsafeThrow "norm Addition inconsistency"
  Left children' -> Left $ Internal Addition children'
norm (Internal IfThenElse [i, l, r]) = case normalizeChildren [i, l, r] of
  Right [Leaf (BoolLeaf i'), Leaf (IntLeaf a), Leaf (IntLeaf b)] ->
    Right $ Leaf $ IntLeaf $ if i' then a else b
  Right [Leaf (BoolLeaf i'), Leaf (BoolLeaf a), Leaf (BoolLeaf b)] ->
    Right $ Leaf $ BoolLeaf $ if i' then a else b
  Right [Leaf (BoolLeaf _), _, _] -> unsafeThrow "non-matching if-then-else branches"
  Right _ -> unsafeThrow "if branching on non-boolean"
  Left children' -> Left $ Internal IfThenElse children'
norm (Internal Parens [x]) = norm x
norm (Internal Annot [tm, _]) = norm tm
norm i@(Internal Eq [x, y]) = case normalizeChildren [x, y] of
  Right [Leaf (IntLeaf a), Leaf (IntLeaf b)] -> Right $ Leaf (BoolLeaf (a == b))
  Right [Leaf (BoolLeaf a), Leaf (BoolLeaf b)] -> Right $ Leaf (BoolLeaf (a == b))
  Right [x', y'] -> unsafeThrow $ "comparison type error (" <> show x' <> "), (" <> show y' <> ")"
  Right _ -> unsafeThrow "norm Eq inconsistency"
  Left children' -> Left $ Internal Eq children'
norm l@(Leaf _) = Right l
norm h@(Hole _) = Left h
norm c@(Conflict _) = Left c
norm _ = unsafeThrow "evaluation error"

-- | Just normalize without indicated whether it's fully noralized or not
norm' :: LangSyntax -> LangSyntax
norm' = norm >>> either id id

inf :: LangSyntax -> LangSyntax
-- Making the assumption that it's well-typed
inf (Internal IfThenElse [_, l, _]) = inf l
inf (Internal IfThenElse _) = unsafeThrow "deeply broken inf (Internal IfThenElse _)"
inf (Internal Addition _) = Leaf IntTy
inf (Internal Parens [x]) = inf x
inf (Internal Parens _) = unsafeThrow "deeply broken inf (Internal Parens _)"
inf (Internal Eq _) = Leaf BoolTy
inf (Internal ArrTy _) = Leaf TyTy
inf (Internal Annot [_, ty]) = ty
inf (Internal Annot _) = unsafeThrow "invariant violation: inf (Internal Annot _)"
inf (Leaf (BoolLeaf _)) = Leaf BoolTy
inf (Leaf (IntLeaf _)) = Leaf IntTy
inf (Leaf IntTy) = Leaf TyTy
inf (Leaf BoolTy) = Leaf TyTy
inf (Leaf TyTy) = Leaf TyTy
inf (Hole _) = Hole "inferred hole type"
inf (Conflict _) = Hole "inferred conflict type"

unsafeUpdateChild :: {term :: LangSyntax, child :: LangSyntax} -> Int -> LangSyntax
unsafeUpdateChild {term, child} i = case term of
  Internal value children -> Internal value (spliceArr children i 1 [child])
  _ -> unsafeThrow "unsafeUpdateChild to non-Internal node"


-- | Try to unify both of these
-- TODO should we return Conflict instead of Nothing?
unify :: LangSyntax -> LangSyntax -> Maybe LangSyntax

-- leaves are easy
unify (Leaf IntTy) (Leaf IntTy) = Just int
unify (Leaf BoolTy) (Leaf BoolTy) = Just bool
unify (Leaf TyTy) (Leaf TyTy) = Just ty
unify u@(Leaf (BoolLeaf x)) (Leaf (BoolLeaf y)) =
  if x == y then Just u else Nothing
unify u@(Leaf (IntLeaf x)) (Leaf (IntLeaf y)) =
  if x == y then Just u else Nothing

-- internal nodes a bit harder
unify (Internal Parens [x]) r = unify x r
unify l (Internal Parens [x]) = unify l x

unify (Internal ArrTy [l1, r1]) (Internal ArrTy [l2, r2]) =
  (\l r -> Internal ArrTy [l, r]) <$> unify l1 l2 <*> unify r1 r2

unify (Internal Annot [tm1, ty1]) (Internal Annot [tm2, ty2]) =
  (\tm ty -> Internal Annot [tm, ty]) <$> unify tm1 tm2 <*> unify ty1 ty2

unify (Internal Annot [tm1, ty]) tm2 =
  (\tm -> Internal Annot [tm, ty]) <$> unify tm1 tm2

unify tm1 (Internal Annot [tm2, ty]) =
  (\tm -> Internal Annot [tm, ty]) <$> unify tm1 tm2

-- these need to be reduced
unify x@(Internal IfThenElse _) r = case norm' x of
  Internal IfThenElse _ -> Nothing
  x' -> unify x' r
unify x@(Internal Addition _) r = case norm' x of
  Internal Addition _ -> Nothing
  x' -> unify x' r
unify x@(Internal Eq _) r = case norm' x of
  Internal Eq _ -> Nothing
  x' -> unify x' r

unify l x@(Internal IfThenElse _) = case norm' x of
  Internal IfThenElse _ -> Nothing
  x' -> unify l x'
unify l x@(Internal Addition _) = case norm' x of
  Internal IfThenElse _ -> Nothing
  x' -> unify l x'
unify l x@(Internal Eq _) = case norm' x of
  Internal IfThenElse _ -> Nothing
  x' -> unify l x'

unify (Hole _) r = Just r
unify l (Hole _) = Just l

unify _ (Conflict _) = Nothing
unify (Conflict _) _ = Nothing

unify _ _ = Nothing

-- | This node's child changed types -- update accordingly.
-- |
-- | There's some subtlety here -- the child node knows what type it needs to
-- | be, so our job is to use any other information we have -- eg from other
-- | child nodes to either accept or conflict that child.
propagateUpType
  :: LangZipper
  -> LangSyntax -- new type
  -> LangZipper
-- assuming the current location can take on this type...
propagateUpType z insideTy = case up z of
  Nothing -> z
  Just {zipper: z', prevLoc} ->
    let outsideTy = case z'.syntax of
          Internal IfThenElse [c, l, r] -> case prevLoc of
            0 -> bool
            1 -> inf r
            2 -> inf l
            _ -> unsafeThrow $ "invariant violation: propagateUpType (Internal IfThenElse), prevLoc: " <> show prevLoc
          Internal IfThenElse _ -> unsafeThrow "invariant violation: propagateUpType (Internal IfThenElse _)"
          Internal Addition [l, r] -> int
          Internal Addition _ -> unsafeThrow "invariant violation: propagateUpType (Internal Addition _)"
          Internal Parens [x] -> inf x
          Internal Parens _ -> unsafeThrow "invariant violation: propagateUpType (Internal Parens _)"
          Internal Eq [l, r] -> case prevLoc of
            0 -> inf r
            1 -> inf l
            _ -> unsafeThrow $ "invariant violation: propagateUpType (Internal Eq), prevLoc: " <> show prevLoc
          Internal Eq _ -> unsafeThrow "invariant violation: propagateUpType (Internal Eq _)"
          Internal ArrTy [l, r] -> ty
          Internal ArrTy _ -> unsafeThrow "invariant violation: propagateUpType (Internal ArrTy _)"
          Internal Annot [_, ty] -> ty
          Internal Annot _ -> unsafeThrow "invariant violation: propagateUpType (Internal Annot _)"
          Conflict {insideTy} -> insideTy
          Leaf _ -> unsafeThrow "invariant violation: propagateUpType (Leaf _)"
          Hole _ -> unsafeThrow "invariant violation: propagateUpType (Hole _)"

    in case unify insideTy outsideTy of
         Just unifiedTy -> propagateUpType z' unifiedTy
         Nothing -> z' {
           -- reversal!
           syntax = propagateDownType {term: z'.syntax, outsideTy: insideTy}
           }


-- XXX probably need to check that term and ty match first
-- | Propagate type information we can use from this type down to the children.
propagateDownType :: {term :: LangSyntax, outsideTy :: LangSyntax} -> LangSyntax
propagateDownType {term, outsideTy} = case term of
  Internal IfThenElse [c, l, r] ->
    Internal IfThenElse
      [ c
      , propagateDownType {term: l, outsideTy}
      , propagateDownType {term: r, outsideTy}
      ]
  -- we can't learn anything new -- addition already constrains its children
  term@(Internal Addition [_, _]) -> case unify outsideTy int of
    Just _ -> term
    Nothing -> Conflict {term, insideTy: int, outsideTy}
  Internal Parens [term] -> propagateDownType {term, outsideTy}
  term@(Internal Eq [l, r]) -> case unify outsideTy bool of
    Just _ -> term
    Nothing -> Conflict {term, insideTy: bool, outsideTy}
  term@(Internal ArrTy [_, _]) -> case unify outsideTy ty of
    Just _ -> term
    Nothing -> Conflict {term, insideTy: ty, outsideTy}
  term@(Internal Annot [subTm, insideTy]) -> case unify outsideTy insideTy of
    Just newTy -> Internal Annot
      [ propagateDownType {term: subTm, outsideTy: newTy}
      , newTy
      ]
    Nothing -> Conflict {term, insideTy, outsideTy}
  Internal _ _ ->
    unsafeThrow "invariant violation: propagateDownType Internal _ _"
  term@(Leaf _) ->
    let insideTy = inf term
    in case unify outsideTy insideTy of
         Just _ -> term
         Nothing -> Conflict {term, outsideTy, insideTy}
  Conflict {term, insideTy} -> case unify insideTy outsideTy of
    Just _ -> term
    Nothing -> Conflict {term, outsideTy, insideTy}
  term@(Hole _) -> Internal Annot [term, outsideTy]


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
      Annot -> "{} : {}"
      -- TODO determine if we can remove the template in Main.purs for this one
      -- Nothing -> "conflict: {{}: expected {} vs actual {}}"
    _ -> unsafeThrow "inconsistency: couldn't get internal template"

  normalize = norm'

  propagateUpType = propagateUpType
  propagateDownType = propagateDownType

  infer = inf
