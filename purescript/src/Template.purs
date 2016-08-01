module Template where

import Prelude
import Data.Array as Array
import Data.Array ((:), filter)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow, gEq)
import Data.Maybe (Maybe(..))
import Data.String (length, split)
import Data.Traversable (Accum, mapAccumL, sequence)


data TemplatePiece
  = TemplateHole
  | TemplateStr String

derive instance genericTemplatePiece :: Generic TemplatePiece
instance showTemplatePiece :: Show TemplatePiece where show = gShow
instance eqTemplatePiece :: Eq TemplatePiece where eq = gEq

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
derive instance genericLightInlineType :: Generic LightInlineType
instance showLightInlineType :: Show LightInlineType where show = gShow
instance eqLightInlineType :: Eq LightInlineType where eq = gEq

------ PARSERS

intersperse :: forall a. a -> Array a -> Array a
intersperse a lst = case lst of
  [x] -> lst
  _ -> case Array.uncons lst of
    Just {head, tail} -> head : a : intersperse a tail
    Nothing -> []

-- TODO are named template holes useful?
mkTemplate :: String -> Template
mkTemplate = split "{}" >>> map TemplateStr >>> intersperse TemplateHole >>> filter (_ /= TemplateStr "")

------

additionTemplate :: Template
additionTemplate = mkTemplate "{} + {}"

parensTemplate :: Template
parensTemplate = mkTemplate "({})"

type AccumState = {fillers :: Array (Array LightInline), pos :: Int}

interpolateTemplate :: Template
                    -> TemplateMeta
                    -> Array (Array LightInline)
                    -> Maybe (Array LightInline)
interpolateTemplate template {key, anchorOffset, focusOffset} fillers =
  let mkPiece :: Int -> String -> Array LightInline
      mkPiece start content = pure
        { ty: InlineInternal
        , key
        , content
        , info: inlineSelection start (length content) anchorOffset focusOffset
        }

      go :: AccumState
         -> TemplatePiece
         -> Accum AccumState (Maybe (Array LightInline))
      go accum piece = case piece of
        TemplateHole -> case Array.uncons accum.fillers of
          Just {head, tail} -> 
            {accum: {fillers: tail, pos: accum.pos}, value: Just head}
          -- fail because we couldn't find a string to fill the hole
          Nothing ->
            {accum, value: Nothing}
        TemplateStr str -> 
          { accum: {fillers: accum.fillers, pos: accum.pos + length str}
          , value: Just (mkPiece accum.pos str)
          }

      result = mapAccumL go {fillers, pos: 0} template

  in case result.accum.fillers of
       -- there shouldn't be any leftovers
       [] -> do
         -- result.value :: Array (Maybe (Array LightInline))
         val <- sequence result.value
         pure (join val)
         -- sequence result.value
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

plusTemplate :: Int -> Maybe Int -> Maybe Int -> Array (Array LightInline) -> Maybe (Array LightInline)
plusTemplate key anchorOffset focusOffset fillers =
  interpolateTemplate additionTemplate {key, anchorOffset, focusOffset} fillers
