module Test.Data.OrdPSQ
  ( psqTest
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.Foldable (foldr)
import Data.List (List(Nil), (:), uncons, reverse, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.NonEmpty ((:|))
import Data.Unfoldable (replicateA)

import Test.QuickCheck ((<?>), quickCheck, class Testable, Result(Success), test)
import Test.QuickCheck.Gen (Gen, frequency, chooseInt, elements)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Data.OrdPSQ.Internal as OrdPSQ

data Action k v
    = Insert k Int v
    | DeleteRandomMember
    | DeleteMin

derive instance eqAction :: (Eq k, Eq v) => Eq (Action k v)

instance showAction :: (Show k, Show v) => Show (Action k v) where
    show (Insert k p v) = "(Insert " <> show k <> " " <> show p <> " " <> show v <> " )"
    show DeleteRandomMember = "DeleteRandomMember"
    show DeleteMin = "DeleteMin"

arbitraryPriority :: Gen Int
arbitraryPriority = chooseInt (-10) 10

applyAction :: forall k v. Ord k => Action k v -> OrdPSQ.OrdPSQ k Int v -> Gen (OrdPSQ.OrdPSQ k Int v)
applyAction (Insert k p v) psq      = pure $ OrdPSQ.insert k p v psq
applyAction DeleteRandomMember psq  = do
    let keys = OrdPSQ.keys psq
    case uncons keys of
        Nothing             -> pure psq
        Just { head, tail } -> do
            key <- elements (head :| A.fromFoldable tail)
            pure $ OrdPSQ.delete key psq
applyAction DeleteMin psq = pure $ case OrdPSQ.minView psq of
    Nothing        -> psq
    Just { queue } -> queue

instance arbitraryAction :: (Arbitrary p, Arbitrary v) => Arbitrary (Action p v) where
    arbitrary = frequency $
        Tuple 10.0 (Insert <$> arbitrary <*> arbitraryPriority <*> arbitrary)
        :|
        ( Tuple 2.0 (pure DeleteRandomMember)
        : Tuple 2.0 (pure DeleteMin)
        : Nil)

newtype TestOrdPSQ k v = TestOrdPSQ (OrdPSQ.OrdPSQ k Int v)

instance arbTestOrdPSQ :: (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (TestOrdPSQ k v) where
    arbitrary = do
        numActions <- chooseInt 0 100
        actions <- replicateA numActions (arbitrary :: Gen (Action k v))
        TestOrdPSQ <$> foldLM (\t a -> applyAction a t) OrdPSQ.empty actions
      where
        foldLM :: forall a b m. Monad m => (b -> a -> m b) -> b -> List a -> m b
        foldLM f z0 xs = foldr f' pure xs z0
          where
            f' x k z = f z x >>= k

data SmallKey = A | B | C | D | E | F | G | H | I | J
derive instance eqSmallKey :: Eq SmallKey
derive instance ordSmallKey :: Ord SmallKey

instance showSmallKey :: Show SmallKey where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"
    show G = "G"
    show H = "H"
    show I = "I"
    show J = "J"

instance arbSmallKey :: Arbitrary SmallKey where
    arbitrary = elements $ A :| [B, C, D, E, F, G, H, I, J]

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

propertyIf :: forall a. Testable a => Boolean -> a -> Gen Result
propertyIf false _ = pure Success
propertyIf true p  = test p

elemEq :: forall k p v. Eq k => Eq p => Eq v => OrdPSQ.ElemRec k p v -> OrdPSQ.ElemRec k p v -> Boolean
elemEq x y = x.key == y.key && x.prio == y.prio && x.value == y.value

showElem :: forall k p v. Show k => Show p => Show v => OrdPSQ.ElemRec k p v -> String
showElem rec =
    "{ key: " <> show rec.key
    <> ", prio: " <> show rec.prio
    <> ", value: " <> show rec.value
    <> " }"

infix 2 propertyIf as ==>

psqTest :: forall eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
psqTest = do
    log "Test eq instance"
    quickCheck $ \k p v (TestOrdPSQ t) ->
        smallKeyToNumberMap t == smallKeyToNumberMap t && OrdPSQ.insert k p v t == OrdPSQ.insert k p v t
        <?> ("q: " <> show t)

    log "Test build OrdPSQ from Foldable"
    quickCheck \xs ->
        propertyFromFoldable xs
        <?> ("foldable: " <> show xs)

    log "Test size of OrdPSQ"
    quickCheck $ \(TestOrdPSQ t) ->
        OrdPSQ.size (t :: OrdPSQ.OrdPSQ Int Int Char) == length (OrdPSQ.toAscList t)
        <?> ("q: " <> show t)

    log "Test property deleteMin"
    quickCheck \(TestOrdPSQ t) ->
        propertyDeleteMin t
        <?> ("q: " <> show t)

    log "Test singleton"
    quickCheck $ \k p v ->
        OrdPSQ.insert k p v OrdPSQ.empty == OrdPSQ.singleton (smallKey k) (number p) (charId v)

    log "Test insert size queue"
    quickCheck $ \k p v (TestOrdPSQ t) ->
        propertyInsertSize k p v t
        <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v <> ", q: " <> show t)

    log "Test inserting into empty queue"
    quickCheck $ \k p v ->
        OrdPSQ.lookup (smallKey k) (OrdPSQ.insert k p v OrdPSQ.empty) == Just (Tuple (number p) (charId v))
        <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v)

    log "Test member lookup"
    quickCheck $ \k (TestOrdPSQ t) ->
        propMemberLookup k t
        <?> ("k: " <> show k <> ", queue: " <> show t)

    log "Test lookup delete"
    quickCheck $ \k p v (TestOrdPSQ t) ->
        (OrdPSQ.lookup (smallKey k) t == Nothing) ==>
        (OrdPSQ.delete k (OrdPSQ.insert k p (charId v) t) == t)

    log "Test delete non member"
    quickCheck $ \k (TestOrdPSQ t) ->
        (OrdPSQ.lookup (smallKey k) t == Nothing) ==>
        (OrdPSQ.delete k t == (t :: OrdPSQ.OrdPSQ SmallKey Int Char))

    log "Test Insert deleteView"
    quickCheck $ \k p v (TestOrdPSQ t) ->
        propertyInsertDeleteView t k p v
        <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v <> ", q: " <> show t)

    log "Test property alter"
    quickCheck $ \k (TestOrdPSQ t) ->
        propertyAlter t k
        <?> ("k: " <> show k <> ", q: " <> show t)

    log "Test empty"
    do
        let psqEmpty = OrdPSQ.empty :: OrdPSQ.OrdPSQ Int Int Char
        quickCheck (OrdPSQ.size psqEmpty == 0 <?> "invalid size - size should 0 for empty queue")

    log "Test findMin"
    do
        let rpsq' = OrdPSQ.fromFoldable [OrdPSQ.elemRec 5 101 'a', OrdPSQ.elemRec 3 100 'b']
        quickCheck (isNothing (OrdPSQ.findMin (OrdPSQ.empty :: OrdPSQ.OrdPSQ Int Int Char)) <?> "Invalid findMin - empty case")
        quickCheck ((elemEq (OrdPSQ.elemRec 3 100 'b') <$> OrdPSQ.findMin rpsq') == Just true <?> "Invalid findMin - 3 100 b")
