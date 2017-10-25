module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Data.Ui (ComponentEffects, main) as Test
import Test.Spec (SpecEffects)
import Test.Spec.Mocha (MOCHA)

main ∷ Eff (SpecEffects (Test.ComponentEffects (mocha ∷ MOCHA))) Unit
main = Test.main
