module Template where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length, split)
import Data.Traversable (Accum, mapAccumL, sequence)

data TemplatePiece
  = TemplateHole
  | TemplateStr String

type Template = Array TemplatePiece

type TemplateMeta =
  { key :: Int
  , anchorOffset :: Maybe Int
  , focusOffset :: Maybe Int
  }

type InlineInfo = { anchor :: Maybe Int , focus :: Maybe Int }

type LightInline =
  { ty :: LightInlineType
  , key :: Int
  , content :: String
  , info :: InlineInfo
  }

data LightInlineType = InlineInternal | InlineLeaf | InlineHole | InlineConflict

------ PARSERS

intersperse :: forall a. a -> Array a -> Array a
intersperse a lst = case lst of
  [x] -> lst
  _ -> case Array.uncons lst of
    Just {head, tail} -> head : a : intersperse a tail
    Nothing -> []

-- TODO are named template holes useful?
mkTemplate :: String -> Template
mkTemplate = split "{}" >>> map TemplateStr >>> intersperse TemplateHole

------

additionTemplate :: Template
additionTemplate = mkTemplate "{} + {}"

parensTemplate :: Template
parensTemplate = mkTemplate "({})"

type AccumState = {fillers :: Array LightInline, pos :: Int}

interpolateTemplate :: Template -> TemplateMeta -> Array LightInline -> Maybe (Array LightInline)
interpolateTemplate template {key, anchorOffset, focusOffset} fillers =
  let mkPiece :: Int -> String -> LightInline
      mkPiece start content = {ty: InlineInternal, key, content, info: inlineSelection start (length content) anchorOffset focusOffset}
      go :: AccumState -> TemplatePiece -> Accum AccumState (Maybe LightInline)
      go accum piece = case Array.uncons accum.fillers of
        Just {head, tail} -> case piece of
          TemplateHole -> {accum: {fillers: tail, pos: accum.pos}, value: Just head}
          _            -> {accum, value: Nothing}
        Nothing -> case piece of
          TemplateStr str -> {accum: {fillers: accum.fillers, pos: accum.pos + length str}, value: Just (mkPiece accum.pos str)}
          _               -> {accum, value: Nothing}
      result = mapAccumL go {fillers, pos: 0} template
  in case result.accum.fillers of
       -- there shouldn't be any leftovers
       [] -> sequence result.value
       _ -> Nothing


zipTemplate :: forall a. Template -> Array a -> Maybe (Array (Either String a))
zipTemplate template fillers =
   let go :: Array a -> TemplatePiece -> Accum (Array a) (Maybe (Either String a))
       go accum TemplateHole = case Array.uncons accum of
         Just {head, tail} -> {accum: tail, value: Just (Right head)}
         Nothing -> {accum, value: Nothing}
       go accum (TemplateStr str) = {accum, value: Just (Left str)}
       result = mapAccumL go fillers template
   in case result.accum of
        [] -> sequence result.value
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

plusTemplate :: Int -> Maybe Int -> Maybe Int -> Array LightInline -> Either String (Array LightInline)
plusTemplate key anchorOffset focusOffset [l, r] = Right
  [ {ty: InlineInternal, key, content: "(", info: inlineSelection 0 1 anchorOffset focusOffset}
  , l
  , {ty: InlineInternal, key, content: " + ", info: inlineSelection 1 3 anchorOffset focusOffset}
  , r
  , {ty: InlineInternal, key, content: ")", info: inlineSelection 4 1 anchorOffset focusOffset}
  ]
plusTemplate _ _ _ arr = Left $ "inconsistency: plus template expected two children, received " <> show (Array.length arr)
