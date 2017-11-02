module Test.Data.IntPSQ
  ( intPsqTest
  ) where

import Prelude

import Data.List (List(Nil), reverse, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Test.Unit.Assert (shouldEqual)
import Test.Unit (describe, it)
import Test.Unit.QuickCheck (quickCheck)

import Test.Data.PSQ.Utils (TestIntPSQ(..), PSQTestSuite)
import Data.IntPSQ.Internal as P


checkEqInstance :: Int -> Int -> Char -> TestIntPSQ Char -> Boolean
checkEqInstance k p v (TestIntPSQ t) =
    P.insert k p v t == P.insert k p v t

propertyFromFoldable :: List (Tuple Int (Tuple Int Char)) -> Boolean
propertyFromFoldable xs =
    P.fromFoldable (fromTuple <$> xs) == P.fromFoldable (fromTuple <$> reverse xs)
  where
    fromTuple :: Tuple Int (Tuple Int Char) -> P.ElemRec Int Char
    fromTuple (Tuple k (Tuple p v)) = P.elemRec k p v

propertyDeleteMin :: TestIntPSQ Char -> Boolean
propertyDeleteMin (TestIntPSQ t) =
    let t' = P.deleteMin t
    in if P.null t
    then t == t'
    else case P.findMin t of
            Nothing -> false
            Just rec -> P.size t' == P.size t - 1
                        && P.member rec.key t
                        && not (P.member rec.key t')

intPsqTest :: forall eff. PSQTestSuite eff
intPsqTest = describe "IntPSQ" do
    it "size" do
        P.null (P.empty :: P.IntPSQ Int Unit) `shouldEqual` true
        P.null (P.singleton 3 100 'a') `shouldEqual` false

    it "empty" do
        (P.keys (P.empty :: P.IntPSQ Int Unit)) `shouldEqual` Nil

    describe "property" do
        describe "property eq instance" do
            it "basic operation" do
                quickCheck checkEqInstance

            it "Reflexity" do
                quickCheck \(TestIntPSQ t) ->
                    ((t :: P.IntPSQ Int Char) == t) == true

            it "symmetry" do
                quickCheck \(TestIntPSQ t1) (TestIntPSQ t2) ->
                    ((t1 :: P.IntPSQ Int Char) == t2) == (t2 == t1)

            it "transitivity" do
                quickCheck \k p v ->
                    let t1 = P.singleton k p v :: P.IntPSQ Int Char
                        t2 = P.singleton k p v
                        t3 = P.singleton k p v
                    in t1 == t2 && t2 == t3 && t1 == t3

        it "build IntPSQ from Foldable" do
            quickCheck propertyFromFoldable

        it "Size return correct" do
            quickCheck $ \(TestIntPSQ t) ->
                P.size (t :: P.IntPSQ Int String) == length (P.toList t)

        it "property deleteMin" do
            quickCheck propertyDeleteMin
