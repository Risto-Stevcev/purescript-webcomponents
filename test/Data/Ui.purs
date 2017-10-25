module Test.Data.Ui where

import Data.Ui

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Control.Semigroupoid (compose)
import DOM (DOM)
import DOM.Classy.Element (setAttribute)
import DOM.Classy.Node (appendChild, removeChild)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLDocument, HTMLElement, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Types (Node)
import Data.Maybe (fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (<>), (>=>), (>>=))
import Test.Spec (SpecEffects, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (MOCHA, runMocha)

infixr 9 compose as ∘

foreign import data ComponentEvents ∷ Type

type ComponentEffects e = (st ∷ ST ComponentEvents, dom ∷ DOM, console ∷ CONSOLE | e)


doc ∷ ∀ e. Eff (dom ∷ DOM | e) HTMLDocument
doc = window >>= document


getBody ∷ ∀ e. Eff (dom ∷ DOM | e) HTMLElement
getBody = do
  doc' ← doc
  body' ← body doc'
  pure $ unsafePartial $ fromJust $ body'


logRef
  ∷ ∀ e
  . STRef ComponentEvents (Array String)
  → String
  → Eff (st ∷ ST ComponentEvents | e) Unit
logRef ref value = do
  _ ← modifySTRef ref (_ <> [value])
  pure unit


logAttr
  ∷ ∀ e
  . STRef ComponentEvents (Array String)
  → String
  → String
  → String
  → Eff (st ∷ ST ComponentEvents | e) Unit
logAttr ref attribute oldValue newValue =
  logRef ref $
    "attributeChangedCallback: " <>
    "{ attribute: " <> attribute <>
    ", oldValue: "  <> oldValue  <>
    ", newValue: "  <> newValue  <> " " <>
    "}"


setupElement
  ∷ ∀ e sym. IsSymbol sym ⇒ CustomElement sym → Eff (dom ∷ DOM | e) Node
setupElement elem = do
   doc' ← doc
   el ← createElement (reflectSymbol (SProxy ∷ SProxy sym)) (htmlDocumentToDocument doc')
   body' ← getBody
   _ ← appendChild el body'
   _ ← setAttribute "foobar" "hello" el
   _ ← setAttribute "bazqux" "world" el
   removeChild el body'


setupFirst
  ∷ ∀ e
  . Eff (ComponentEffects e) (STRef ComponentEvents (Array String))
setupFirst = do
  ref ← newSTRef []
  elem ← on "foobaz" (\_ → \_ → \_ → log "foobar") $ customElement
    ["foobar"]
    (\_ → logRef ref "constructor")
    (\_ → logRef ref "connectedCallback")
    (\_ → logRef ref "disconnectedCallback")
    (\_ → logAttr ref)
    (\_ → logRef ref "adoptedCallback")

  registerElement (SProxy ∷ SProxy "my-foo") elem
  _ ← setupElement elem

  pure ref


setupSecond
  ∷ ∀ e
  . Eff (ComponentEffects e) (STRef ComponentEvents (Array String))
setupSecond = do
  ref ← newSTRef []
  let elem = (element ∘ const ∘ logRef ref) "constructor"

  _ ← setup ref elem
  registerElement (SProxy ∷ SProxy "my-bar") elem
  _ ← setupElement elem

  pure ref

  where
    setup ref = on "foobar" (\_ → logAttr ref "foobar")
            >=> on "bazqux" (\_ → logAttr ref "bazqux")
            >=> onMount     (\_ → logRef ref "connectedCallback")
            >=> onUnmount   (\_ → logRef ref "disconnectedCallback")


main
  ∷ Eff (SpecEffects (ComponentEffects (mocha ∷ MOCHA))) Unit
main = do
  runMocha do
    describe "purescript-ui" do
      describe "customElement" do
        it "should call the appropriate callbacks for the web component" do
          actual ← liftEff $ setupFirst >>= readSTRef
          let expected = [ "constructor"
                         , "connectedCallback"
                         , "attributeChangedCallback: " <>
                           "{ attribute: foobar, oldValue: null, newValue: hello }"
                         , "disconnectedCallback"
                         ]
          actual `shouldEqual` expected

      describe "element" do
        it "should work with all the combinators" do
          actual ← liftEff $ setupSecond >>= readSTRef
          let expected = [ "constructor"
                         , "connectedCallback"
                         , "attributeChangedCallback: " <>
                           "{ attribute: foobar, oldValue: null, newValue: hello }"
                         , "attributeChangedCallback: " <>
                           "{ attribute: bazqux, oldValue: null, newValue: world }"
                         , "disconnectedCallback"
                         ]
          actual `shouldEqual` expected
