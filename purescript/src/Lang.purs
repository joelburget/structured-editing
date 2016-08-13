module Lang where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(..), isLeft, either)
import Data.Foldable (any)
import Data.Foreign (Foreign, ForeignError(JSONError), readString, toForeign)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Function.Uncurried (mkFn1, Fn1)
import Data.Generic (class Generic, gShow, gEq)
import Data.Map as Map
import Data.Map (Map, member)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple
import Data.String as String
import Data.String (length)
import Partial.Unsafe (unsafePartial)
import Data.Int as I
import Data.List ((:))
import Data.Foldable (any)
import Data.Foreign (Foreign, ForeignError(JSONError), readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Function.Uncurried (mkFn1, Fn1)
import Data.Array as Array
import Data.Array.Partial (unsafeIndex)

import Interface as Interface
import Syntax
import Template
import Path
import Util


-- specialize a bunch of data structures for this language
type LangSyntax = Syntax Internal Leaf
type LangZipper = SyntaxZipper Internal Leaf
type LangZoomed = ZoomedSZ Internal Leaf
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


instance intBoolIsTemplated :: TemplatedTree Internal Leaf where
  getLeafTemplate = case _ of
    Leaf (BoolLeaf b) -> show b
    Leaf (IntLeaf i) -> show i
    Leaf IntTy -> "int"
    Leaf BoolTy -> "bool"
    Leaf TyTy -> "type"
    _ -> unsafeThrow "inconsistency: couldn't get leaf template"

  getInternalTemplate = case _ of
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

instance intBoolIsLang :: Lang Internal Leaf where
  normalize = norm'

  propagateUpType = propagateUpType
  propagateDownType = propagateDownType

  infer = inf

instance intBoolIsOperate :: Operational Internal Leaf where
  doOperate = doOperate

initSelectSyntax :: Fn1 Foreign (Either String LangZoomed)
initSelectSyntax = Interface.initSelectSyntax


-- OPERATE

leafKeywords :: Map String LangSyntax
leafKeywords = map Leaf $ Map.fromFoldable
  [ Tuple "true" (BoolLeaf true)
  , Tuple "false" (BoolLeaf false)
  , Tuple "type" TyTy
  , Tuple "int" IntTy
  , Tuple "bool" BoolTy
  ]

recognizeLeafKeyword :: String -> LangPast -> CursorPath -> LangZipper
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
  in proposeNew syntax z

internalKeywords :: Map String LangSyntax
internalKeywords = Map.fromFoldable
  [ Tuple "(" (Internal Parens [Hole "_"])
  , Tuple "+" (Internal Addition [Hole "_", Hole "_"])
  , Tuple "if" (Internal IfThenElse [Hole "_", Hole "_", Hole "_"])
  , Tuple "==" (Internal Eq [Hole "_", Hole "_"])
  , Tuple "->" (Internal ArrTy [Hole "_", Hole "_"])
  , Tuple ":" (Internal Annot [Hole "_", Hole "_"])
  ]

recognizeInternalKeyword :: String -> LangPast -> CursorPath -> LangZipper
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

anchorIsAllLeft :: CursorPath -> Boolean
anchorIsAllLeft (PathOffset 0) = true
anchorIsAllLeft (PathCons 0 rest) = anchorIsAllLeft rest
anchorIsAllLeft _ = false

focusIsAllRight :: forall a b. Lang a b => CursorPath -> Syntax a b -> Boolean
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
            Tuple _ _ -> NoSuggestion

-- TODO does this actually use the Left option?
resolveConflictAt :: LangZipper -> Action -> Either String LangZipper
resolveConflictAt z action =
  let bmark = bookmark z
      loc = case action of
        TakeOutside loc -> loc
        TakeInside loc -> loc
        _ -> unsafeThrow
          "invariant violation: resolveConflictAt called with unexpected action"
      z' = moveTo (zipUp z) loc
      z'' = case Tuple z'.syntax action of
        Tuple (Conflict {term, outsideTy}) (TakeOutside _) ->
          let syntax = propagateDownType {term, outsideTy}
          in z' {syntax = syntax}
        Tuple (Conflict {term, insideTy}) (TakeInside _) ->
          let nonConflicting = z' {syntax = term}
          in propagateUpType nonConflicting insideTy
        Tuple _ _ -> unsafeThrow
          "invariant violation: resolveConflictAt: unexpected conflict / action"
      z''' = moveTo (zipUp z'') bmark.zoom
  in Right $ z''' {anchor = bmark.anchor, focus = bmark.focus}

updateTypeOf :: LangZipper -> LangSyntax -> LangZipper
updateTypeOf z outsideTy =
      -- now march down to update the children
  let syntax = propagateDownType
        { term: z.syntax
        -- XXX I *don't* think this is right
        , outsideTy
        }
  in z {syntax = syntax}

-- | Two easy cases:
-- | * we've selected the entirety of a hole / conflict -- just navigate to the
-- |   next.
-- | * the cursor is between hole / conflict -- navigate to the next
-- |
-- | Harder cases:
-- | * cursor within a hole -- select the whole thing?
-- | * selection not exactly aligned with a hole -- ?
tabNavigate :: LangZipper -> Action -> Either String LangZipper
tabNavigate zipper action = Left "tabbing not yet implemented!"

-- TODO this should be part of the language definition
doOperate :: LangZipper -> Action -> Either String LangZipper
doOperate zipper@{syntax, anchor, focus} action = case action of
  (TakeOutside _) -> resolveConflictAt zipper action
  (TakeInside _) -> resolveConflictAt zipper action
  (Tab _) -> tabNavigate zipper action
  _ -> if anchor == focus
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
                     operateOffsetsInNode
                       zipper
                       action
                       (min aOff fOff)
                       (max aOff fOff)
                   _ -> Left "spanning actions not yet implemented (1)"

-- | Kind of cheating -- zoom and unwrap
zoomIn' :: forall a b. SyntaxZipper a b -> SyntaxZipper a b
zoomIn' z = case zoomIn z of ZoomedSZ z' -> z'

-- throwUserMessage :: forall a. String -> Operate a
throwUserMessage :: String -> Either String LangZipper
throwUserMessage = Left

-- | Propose a new node.
-- |
-- | Why do you need to "propose"? Because the update could conflict with other
-- | values.
-- |
-- | Uses propagateUpType to move up the zipper.
proposeNew :: LangSyntax -> LangZipper -> LangZipper
proposeNew syntax z =
  let inferredTy = infer syntax
      z' = z {syntax = syntax}
  in zoomIn' $ propagateUpType z' inferredTy

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
          in proposeNew (Leaf (IntLeaf n)) z'
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


suggestionsToForeign :: SelectionSuggestions -> Foreign
suggestionsToForeign MoveStart = toForeign "move-start"
suggestionsToForeign MoveFinish = toForeign "move-finish"
suggestionsToForeign MoveBoth = toForeign "move-both"
suggestionsToForeign NoSuggestion = toForeign "no-suggestion"

type SelectionInfo =
  { selectionSuggestions :: Foreign
  , evaluated :: LangSyntax
  }

selectionInfo :: Fn1 LangZipper SelectionInfo
selectionInfo = mkFn1 \z ->
  case zoomIn z of
    ZoomedSZ zz ->
      { selectionSuggestions: suggestionsToForeign (suggestCoherentSelection z)
      , evaluated: normalize z.syntax
      }
