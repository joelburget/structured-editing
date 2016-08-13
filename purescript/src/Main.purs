module Main where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (snoc, (..), (:))
import Data.List as List
import Data.Map as Map
import Data.String as String
import Operate as Operate
import Control.Monad.State (State, modify, get, evalState, execState)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Generic (readGeneric)
import Data.Function.Uncurried (mkFn2, Fn2, mkFn1, Fn1)
import Data.Generic (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (sequence)
import Path (CursorPath, PathStep, subPath, getOffset)
import Syntax (ZoomedSZ(ZoomedSZ), normalize, zoomIn, syntaxHoles, syntaxConflicts, zipUp, makePath)
import Generic (myOptions)
import Lang (LangZipper, LangSyntax, ZoomedLang, LangConflictInfo, Internal, Leaf)
import Operate (suggestCoherentSelection, SelectionSuggestions(..))
import Serialize


initSelectSyntax :: Fn1 Foreign (Either String ZoomedLang)
initSelectSyntax = mkFn1 \foreignSelectSyntax -> do
  raw <- lmap show (read foreignSelectSyntax)
  unrawSelectSyntax raw

genContentState :: Fn1 LangZipper Foreign
genContentState = mkFn1 \zipper ->
  let top = zipUp zipper
      -- TODO why the Justs?
      yieldsContent = contentFromSyntax top.syntax (Just top.anchor) (Just top.focus)
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (contentAndKeymapping.inlines)
  in toForeign contentState

genDisplayContentState :: Fn1 LangSyntax Foreign
genDisplayContentState = mkFn1 \syntax ->
  let yieldsContent = contentFromSyntax syntax Nothing Nothing
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (contentAndKeymapping.inlines)
  in toForeign contentState

operate :: Fn2 LangZipper Foreign (Either String LangZipper)
operate = mkFn2 \zipper foreignAction -> do
  action <- lmap show (read foreignAction)
  Operate.operate zipper action


-- wrap this record so we can read it in `setEndpoints`
newtype WrappedAnchorFocus = WrappedAnchorFocus {anchor :: Int, focus :: Int}

derive instance genericWrappedAnchorFocus :: Generic WrappedAnchorFocus
instance foreignWrappedAnchorFocus :: IsForeign WrappedAnchorFocus where
  read = readGeneric myOptions

setEndpoints :: Fn2 LangZipper Foreign (Either String ZoomedLang)
setEndpoints = mkFn2 \zipper foreignEndpoints -> do
  let top = zipUp zipper
  WrappedAnchorFocus {anchor: aOffset, focus: fOffset}
    <- lmap show (read foreignEndpoints)
  anchor <- makePath top.syntax aOffset
  focus <- makePath top.syntax fOffset
  let zipper' = top {anchor = anchor, focus = focus}
  pure (zoomIn zipper')

listLocalHoles :: Fn1 LangZipper (Array LangSyntax)
listLocalHoles = mkFn1 (_.syntax >>> syntaxHoles)

listAllHoles :: Fn1 LangZipper (Array LangSyntax)
listAllHoles = mkFn1 (zipUp >>> _.syntax >>> syntaxHoles)

listAllConflicts :: Fn1
  LangZipper
  (Array {conflict :: LangSyntax, loc :: Array PathStep})
listAllConflicts = mkFn1 (zipUp >>> _.syntax >>> syntaxConflicts)

listLocalConflicts :: Fn1
  LangZipper
  (Array {conflict :: LangSyntax, loc :: Array PathStep})
listLocalConflicts = mkFn1 (_.syntax >>> syntaxConflicts)

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
