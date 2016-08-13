module Interface where

import Prelude
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (snoc, (..), (:))
import Data.List as List
import Data.Map as Map
import Data.String as String
import Control.Monad.State (State, modify, get, evalState, execState)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Generic (readGeneric)
import Data.Function.Uncurried (mkFn2, Fn2, mkFn1, Fn1)
import Data.Generic (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (sequence)
import Generic (myOptions)

import Path (NodePath, CursorPath(..), PathStep, subPath, getOffset)
import Syntax (class TemplatedTree, class Lang, Syntax, SyntaxZipper, ZoomedSZ(ZoomedSZ), normalize, zoomIn, syntaxHoles, syntaxConflicts, zipUp, makePath)
import Serialize


initSelectSyntax
  :: forall i l. (IsForeign i, IsForeign l, TemplatedTree i l)
  => Fn1 Foreign (Either String (ZoomedSZ i l))
initSelectSyntax = mkFn1 \foreignSelectSyntax -> do
  raw <- lmap show (read foreignSelectSyntax)
  unrawSelectSyntax raw

genContentState
  :: forall i l. TemplatedTree i l
  => Fn1 (SyntaxZipper i l) Foreign
genContentState = mkFn1 \zipper ->
  let top = zipUp zipper
  in genHelper top.syntax top.anchor top.focus

genDisplayContentState
  :: forall i l. TemplatedTree i l
  => Fn1 (Syntax i l) Foreign
genDisplayContentState = mkFn1 \syntax ->
  genHelper syntax CursorOutOfScope CursorOutOfScope

-- Just a helper for `genContentState` / `genDisplayContentState`
genHelper
  :: forall i l. TemplatedTree i l
  => Syntax i l
  -> CursorPath
  -> CursorPath
  -> Foreign
genHelper syntax anchor focus =
  let yieldsContent = contentFromSyntax syntax anchor focus
      contentAndKeymapping = evalState yieldsContent 0
      contentState = blockFromContent (contentAndKeymapping.inlines)
  in toForeign contentState


-- | Attempt to read a foreign value, handling errors using the specified function
readProp' :: forall a. IsForeign a => String -> Foreign -> Either String a
readProp' str value = either
  (\_ -> Left $ "failed to read " <> str)
  Right
  (readProp str value)


setEndpoints
  :: forall i l. TemplatedTree i l
  => Fn2 (SyntaxZipper i l) Foreign (Either String (ZoomedSZ i l))
setEndpoints = mkFn2 \zipper foreignEndpoints -> do
  let top = zipUp zipper
  aOffset <- readProp' "anchor" foreignEndpoints
  fOffset <- readProp' "focus" foreignEndpoints
  anchor <- makePath top.syntax aOffset
  focus <- makePath top.syntax fOffset
  let zipper' = top {anchor = anchor, focus = focus}
  pure (zoomIn zipper')

listLocalHoles
  :: forall i l. TemplatedTree i l
  => Fn1 (SyntaxZipper i l) (Array (Syntax i l))
listLocalHoles = mkFn1 (_.syntax >>> syntaxHoles)

listAllHoles
  :: forall i l. TemplatedTree i l
  => Fn1 (SyntaxZipper i l) (Array (Syntax i l))
listAllHoles = mkFn1 (zipUp >>> _.syntax >>> syntaxHoles)

listAllConflicts
  :: forall i l. TemplatedTree i l
  => Fn1 (SyntaxZipper i l)
         (Array {conflict :: (Syntax i l), loc :: NodePath})
listAllConflicts = mkFn1 (zipUp >>> _.syntax >>> syntaxConflicts)

listLocalConflicts
  :: forall i l. TemplatedTree i l
  => Fn1 (SyntaxZipper i l)
         (Array {conflict :: (Syntax i l), loc :: NodePath})
listLocalConflicts = mkFn1 (_.syntax >>> syntaxConflicts)
