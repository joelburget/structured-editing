module Test.Template where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Maybe
import Test.Unit (Test, suite, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Template

newtype LIEq = LIEq LightInline
instance lieq :: Eq LIEq where
  eq (LIEq li1) (LIEq li2) = li1.ty == li2.ty
    && li1.key == li2.key
    && li1.content == li2.content
    && li1.info.anchor == li2.info.anchor
    && li1.info.focus == li2.info.focus
instance showLIEq :: Show LIEq where
  show _ = "LIEq TODO"

assertTemplateEq :: forall e. Array LightInline
                 -> Maybe (Array LightInline)
                 -> Test e
assertTemplateEq expected found = Assert.equal
  (Just (LIEq <$> expected))
  ((map <<< map) LIEq found)

mkTemplateSuite = suite "mkTemplate" do
  test "{} + {}" do
    Assert.equal
      [TemplateHole, TemplateStr " + ", TemplateHole]
      (mkTemplate "{} + {}")

templateSuite = suite "templating" do
  test "0 + 1" do
    let info = {anchor: Nothing, focus: Nothing}
        l = {ty: InlineLeaf, key: 0, content: "0", info}
        r = {ty: InlineLeaf, key: 1, content: "1", info}
        children = [[l], [r]]
        key = 1
    assertTemplateEq
      [ l
      , {ty: InlineInternal
          , key
          , content: " + "
          , info: inlineSelection 1 3 Nothing Nothing
        }
      , r
      ]
      (plusTemplate 1 Nothing Nothing children)
