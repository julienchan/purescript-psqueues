module Test.Data.OrdPSQ
  ( ordPsqTest
  ) where

import Prelude

import Data.List (List(Nil), reverse, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Test.Unit.Assert (shouldEqual)
import Test.Unit (describe, it)
import Test.Unit.QuickCheck (quickCheck)

import Test.Data.PSQ.Utils (TestOrdPSQ(..), SmallKey, PSQTestSuite, (==>))
import Data.OrdPSQ.Internal as OrdPSQ

smallKey :: SmallKey -> SmallKey
smallKey k = k

number :: Int -> Int
number n = n

charId :: Char -> Char
charId c = c

smallKeyToNumberMap :: OrdPSQ.OrdPSQ SmallKey Int String -> OrdPSQ.OrdPSQ SmallKey Int String
smallKeyToNumberMap m = m

propMemberLookup :: SmallKey -> OrdPSQ.OrdPSQ SmallKey Int String -> Boolean
propMemberLookup k t = case OrdPSQ.lookup k t of
    Nothing -> not (OrdPSQ.member k t)
    Just _  -> OrdPSQ.member k t

propertyInsertSize :: SmallKey -> Int -> String -> OrdPSQ.OrdPSQ SmallKey Int String -> Boolean
propertyInsertSize k p s t =
    let t' = OrdPSQ.insert k p s t
    in if OrdPSQ.member k t then OrdPSQ.size t == OrdPSQ.size t' else OrdPSQ.size t' == (OrdPSQ.size t) + 1

propertyDeleteMin :: OrdPSQ.OrdPSQ SmallKey Int Char -> Boolean
propertyDeleteMin t =
    let t' = OrdPSQ.deleteMin t
    in if OrdPSQ.null t
    then t == t'
    else case OrdPSQ.findMin t of
            Nothing -> false
            Just rec -> OrdPSQ.size t' == OrdPSQ.size t - 1
                        && OrdPSQ.member rec.key t
                        && not (OrdPSQ.member rec.key t')

propertyFromFoldable :: List (Tuple Int (Tuple Int Char)) -> Boolean
propertyFromFoldable xs =
    OrdPSQ.fromFoldable (fromTuple <$> xs) == OrdPSQ.fromFoldable (fromTuple <$> reverse xs)
  where
    fromTuple :: Tuple Int (Tuple Int Char) -> OrdPSQ.ElemRec Int Int Char
    fromTuple (Tuple k (Tuple p v)) = OrdPSQ.elemRec k p v

propertyInsertDeleteView :: OrdPSQ.OrdPSQ SmallKey Int Char -> SmallKey -> Int -> Char -> Boolean
propertyInsertDeleteView t k p c = case OrdPSQ.deleteView k (OrdPSQ.insert k p c t) of
    Nothing -> false
    Just rec
        | OrdPSQ.member k t -> rec.prio == p && rec.value == c && OrdPSQ.size rec.queue < OrdPSQ.size t
        | otherwise         -> rec.prio == p && rec.value == c && rec.queue == t

propertyAlter :: OrdPSQ.OrdPSQ SmallKey Int Char -> SmallKey -> Boolean
propertyAlter t k =
    let Tuple _ t' = OrdPSQ.alter f k t
    in case OrdPSQ.lookup k t of
        Just _    -> (OrdPSQ.size t - 1) == OrdPSQ.size t' && OrdPSQ.lookup k t' == Nothing
        Nothing   -> (OrdPSQ.size t + 1) == OrdPSQ.size t' && OrdPSQ.lookup k t' /= Nothing
  where
    f Nothing   = Tuple unit $ Just (Tuple 100 'a')
    f (Just _)  = Tuple unit Nothing

elemEq :: forall k p v. Eq k => Eq p => Eq v => OrdPSQ.ElemRec k p v -> OrdPSQ.ElemRec k p v -> Boolean
elemEq x y = x.key == y.key && x.prio == y.prio && x.value == y.value

showElem :: forall k p v. Show k => Show p => Show v => OrdPSQ.ElemRec k p v -> String
showElem rec =
    "{ key: " <> show rec.key
    <> ", prio: " <> show rec.prio
    <> ", value: " <> show rec.value
    <> " }"

ordPsqTest :: forall eff. PSQTestSuite eff
ordPsqTest =
    describe "Basic functionality" do
        it "size" do
            OrdPSQ.null (OrdPSQ.empty :: OrdPSQ.OrdPSQ Int Char Unit) `shouldEqual` true
            OrdPSQ.null (OrdPSQ.singleton 3 100 'a') `shouldEqual` false

        it "empty" do
            (OrdPSQ.keys (OrdPSQ.empty :: OrdPSQ.OrdPSQ Int Char Unit)) `shouldEqual` Nil

        describe "Property Check" do
            describe "property eq instance" do
                it "Basic operation" do
                    quickCheck $ \k p v (TestOrdPSQ t) ->
                        smallKeyToNumberMap t == smallKeyToNumberMap t
                        && OrdPSQ.insert k p v t == OrdPSQ.insert k p v t

                it "Reflexity" do
                    quickCheck \(TestOrdPSQ t) ->
                        ((t :: OrdPSQ.OrdPSQ SmallKey Int Char) == t) == true

                it "symmetry" do
                    quickCheck \(TestOrdPSQ t1) (TestOrdPSQ t2) ->
                        ((t1 :: OrdPSQ.OrdPSQ SmallKey Int Char) == t2) == (t2 == t1)

                it "transitivity" do
                    quickCheck \k p v ->
                        let t1 = OrdPSQ.singleton k p v :: OrdPSQ.OrdPSQ SmallKey Int Char
                            t2 = OrdPSQ.singleton k p v
                            t3 = OrdPSQ.singleton k p v
                        in t1 == t2 && t2 == t3 && t1 == t3

            it "build OrdPSQ from Foldable" do
                quickCheck propertyFromFoldable

            it "Size return correct" do
                quickCheck $ \(TestOrdPSQ t) ->
                    OrdPSQ.size (t :: OrdPSQ.OrdPSQ Int Int Char) == length (OrdPSQ.toAscList t)

            it "Property deleteMin" do
                quickCheck \(TestOrdPSQ t) -> propertyDeleteMin t

            it "Test singleton" do
                quickCheck $ \k p v ->
                    OrdPSQ.insert k p v OrdPSQ.empty == OrdPSQ.singleton (smallKey k) (number p) (charId v)

            it "Test insert size queue" do
                quickCheck $ \k p v (TestOrdPSQ t) ->
                    propertyInsertSize k p v t

            it "Test inserting into empty queue" do
                quickCheck $ \k p v ->
                    OrdPSQ.lookup (smallKey k) (OrdPSQ.insert k p v OrdPSQ.empty) == Just (Tuple (number p) (charId v))

            it "Test member lookup" do
                quickCheck $ \k (TestOrdPSQ t) -> propMemberLookup k t

            it "Test lookup delete" do
                quickCheck $ \k p v (TestOrdPSQ t) ->
                    (OrdPSQ.lookup (smallKey k) t == Nothing) ==>
                    (OrdPSQ.delete k (OrdPSQ.insert k p (charId v) t) == t)

            it "Test delete non member" do
                quickCheck $ \k (TestOrdPSQ t) ->
                    (OrdPSQ.lookup (smallKey k) t == Nothing) ==>
                    (OrdPSQ.delete k t == (t :: OrdPSQ.OrdPSQ SmallKey Int Char))

            it "Test Insert deleteView" do
                quickCheck $ \k p v (TestOrdPSQ t) -> propertyInsertDeleteView t k p v

            it "Test property alter" do
                quickCheck $ \k (TestOrdPSQ t) -> propertyAlter t k
