module Data.Ui
  ( CustomElement
  , customElement
  , registerElement
  , element
  , on
  , onMount
  , onUnmount
  , onAdopt
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, EffFn4, mkEffFn1, mkEffFn3, mkEffFn4)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)


foreign import data CustomElement ∷ Symbol → Type


foreign import _onMount
  ∷ ∀ sym e
  . EffFn1 e HTMLElement Unit
  → CustomElement sym
  → Eff e (CustomElement sym)

-- | Attaches a connectedCallback to the given CustomElement and returns it for
-- | easier chaining.
-- | This function is side effecting because it mutates the given CustomElement
onMount
  ∷ ∀ sym e
  . (HTMLElement → Eff e Unit)
  → CustomElement sym
  → Eff e (CustomElement sym)
onMount connectedCallback = _onMount (mkEffFn1 connectedCallback)


foreign import _onUnmount
  ∷ ∀ sym e
  . EffFn1 e HTMLElement Unit
  → CustomElement sym
  → Eff e (CustomElement sym)


-- | Attaches a disconnectedCallback to the given CustomElement and returns it
-- | for easier chaining.
-- | This function is side effecting because it mutates the given CustomElement
onUnmount
  ∷ ∀ sym e
  . (HTMLElement → Eff e Unit)
  → CustomElement sym
  → Eff e (CustomElement sym)
onUnmount disconnectedCallback = _onUnmount (mkEffFn1 disconnectedCallback)


foreign import _onAdopt
  ∷ ∀ sym e
  . EffFn1 e HTMLElement Unit
  → CustomElement sym
  → Eff e (CustomElement sym)

-- | Attaches an adoptedCallback to the given CustomElement and returns it for
-- | easier chaining.
-- | This function is side effecting because it mutates the given CustomElement
onAdopt
  ∷ ∀ sym e
  . (HTMLElement → Eff e Unit)
  → CustomElement sym
  → Eff e (CustomElement sym)
onAdopt adoptedCallback = _onAdopt (mkEffFn1 adoptedCallback)


foreign import _on
  ∷ ∀ sym e
  . String
  → EffFn3 e HTMLElement String String Unit
  → CustomElement sym
  → Eff e (CustomElement sym)

-- | Adds the given attribute to observedAttributes if it is not included and
-- | calls the given callback when attributeChangedCallback fires for that
-- | attribute.
-- | This function is side effecting because it mutates the given CustomElement
on
  ∷ ∀ sym e
  . String
  → (HTMLElement → String → String → Eff e Unit)
  → CustomElement sym
  → Eff e (CustomElement sym)
on attribute attributeChangedCallback =
  _on attribute (mkEffFn3 attributeChangedCallback)


foreign import _customElement ∷ ∀ sym e
  . Array String
  → EffFn1 e HTMLElement Unit
  → EffFn1 e HTMLElement Unit
  → EffFn1 e HTMLElement Unit
  → EffFn4 e HTMLElement String String String Unit
  → EffFn1 e HTMLElement Unit
  → CustomElement sym

-- | A rough translation of creating a web component by extending HTMLElement
customElement ∷ ∀ sym e
  . Array String
  → (HTMLElement → Eff e Unit)
  → (HTMLElement → Eff e Unit)
  → (HTMLElement → Eff e Unit)
  → (HTMLElement → String → String → String → Eff e Unit)
  → (HTMLElement → Eff e Unit)
  → CustomElement sym
customElement
  observedAttributes
  constructor
  connectedCallback
  disconnectedCallback
  attributeChangedCallback
  adoptedCallback = _customElement
                      observedAttributes
                      (mkEffFn1 constructor)
                      (mkEffFn1 connectedCallback)
                      (mkEffFn1 disconnectedCallback)
                      (mkEffFn4 attributeChangedCallback)
                      (mkEffFn1 adoptedCallback)


-- | Creates an empty web component to be customized with the combinator functions
element ∷ ∀ e sym. (HTMLElement → Eff e Unit) → CustomElement sym
element constructor =
  _customElement [] (mkEffFn1 constructor) noop noop noop4 noop
  where
    noop = mkEffFn1 (const (pure unit))
    noop4 = mkEffFn4 (\_ → \_ → \_ → \_ → pure unit)


foreign import _registerElement
  ∷ ∀ a e. String → CustomElement a → Eff (dom ∷ DOM | e) Unit

-- | Registers an element in the web components registry.
-- | Any properties of the CustomElement that are changed, such as adding `on`,
-- | `onMount`, or `onUnmount` listeners, must execute before calling this
-- | function in order for them to work
registerElement ∷ ∀ e sym
  . IsSymbol sym
  ⇒ SProxy sym
  → CustomElement sym → Eff (dom ∷ DOM | e) Unit
registerElement = _registerElement <<< reflectSymbol
