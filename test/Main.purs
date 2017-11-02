
module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)

import Test.QuickCheck (QC)
import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Test.Data.OrdPSQ (ordPsqTest)
import Test.Data.IntPSQ (intPsqTest)

main :: forall eff. QC (testOutput :: TESTOUTPUT, avar :: AVAR | eff) Unit
main = runTest $ suite "PSQueus" do
    ordPsqTest
    intPsqTest
