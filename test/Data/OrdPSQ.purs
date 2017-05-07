module Test.Data.OrdPSQ
  ( ordPSQTest
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.Foldable (foldr)
import Data.List (List(Nil), (:), uncons, reverse, length)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (Tuple3, (/\), tuple3, get4, get1)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Unfoldable (replicateA)

import Test.QuickCheck ((<?>), quickCheck, class Testable, Result(Success), test)
import Test.QuickCheck.Gen (Gen, frequency, chooseInt, elements)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Data.OrdPSQ.Internal as PSQ

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

applyAction :: forall k v. Ord k => Action k v -> PSQ.OrdPSQ k Int v -> Gen (PSQ.OrdPSQ k Int v)
applyAction (Insert k p v) psq      = pure $ PSQ.insert k p v psq
applyAction DeleteRandomMember psq  = do
  let keys = PSQ.keys psq
  case uncons keys of
    Nothing -> pure psq
    Just { head, tail } -> do
      key <- elements (head :| A.fromFoldable tail)
      pure $ PSQ.delete key psq
applyAction DeleteMin psq = pure $ case PSQ.minView psq of
  Nothing -> psq
  Just t4 -> get4 t4

instance arbitraryAction :: (Arbitrary p, Arbitrary v) => Arbitrary (Action p v) where
  arbitrary = frequency $
    Tuple 10.0 (Insert <$> arbitrary <*> arbitraryPriority <*> arbitrary)
    :|
    (Tuple 2.0 (pure DeleteRandomMember)
    : Tuple 2.0 (pure DeleteMin)
    : Nil)

newtype TestOrdPSQ k v = TestOrdPSQ (PSQ.OrdPSQ k Int v)

instance arbTestOrdPSQ :: (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (TestOrdPSQ k v) where
  arbitrary = do
    numActions <- chooseInt 0 100
    actions <- replicateA numActions (arbitrary :: Gen (Action k v))
    TestOrdPSQ <$> foldLM (\t a -> applyAction a t) PSQ.empty actions
    where
      foldLM :: forall a b m. Monad m => (b -> a -> m b) -> b -> List a -> m b
      foldLM f z0 xs = foldr f' pure xs z0
        where
          f' x k z = f z x >>= k

instance showTestOrdPSQ :: (Show k, Show v) => Show (TestOrdPSQ k v) where
  show (TestOrdPSQ t) = "(TestOrdPSQ " <> show (PSQ.toAscList t) <> " )"

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

smallKeyToNumberMap :: PSQ.OrdPSQ SmallKey Int String -> PSQ.OrdPSQ SmallKey Int String
smallKeyToNumberMap m = m

propMemberLookup :: SmallKey -> TestOrdPSQ SmallKey String -> Boolean
propMemberLookup k (TestOrdPSQ t) = case PSQ.lookup k t of
  Nothing -> not (PSQ.member k t)
  Just _  -> PSQ.member k t

propertyInsertSize :: SmallKey -> Int -> String -> TestOrdPSQ SmallKey String -> Boolean
propertyInsertSize k p s (TestOrdPSQ t) =
  let t' = PSQ.insert k p s t
  in if PSQ.member k t then PSQ.size t == PSQ.size t' else PSQ.size t' == (PSQ.size t) + 1

propertyDeleteMin :: TestOrdPSQ SmallKey Char -> Boolean
propertyDeleteMin (TestOrdPSQ t) =
  let
    t' = PSQ.deleteMin t
  in
  if PSQ.null t
    then t == t'
    else case PSQ.findMin t of
           Nothing -> false
           Just t3 ->
             PSQ.size t' == PSQ.size t - 1
             && PSQ.member (get1 t3) t
             && not (PSQ.member (get1 t3) t')

propertyFromFoldable :: List (Tuple3 Int Int Char) -> Boolean
propertyFromFoldable xs =
  PSQ.fromFoldable xs == PSQ.fromFoldable (reverse xs)

propertyInsertDeleteView :: TestOrdPSQ SmallKey Char -> SmallKey -> Int -> Char -> Boolean
propertyInsertDeleteView (TestOrdPSQ t) k p c =
  case PSQ.deleteView k (PSQ.insert k p c t) of
    Nothing -> false
    Just (p' /\ c' /\ t' /\ _)
      | PSQ.member k t -> p' == p && c' == c && PSQ.size t' < PSQ.size t
      | otherwise      -> p' == p && c' == c && t' == t

propertyIf :: forall a. Testable a => Boolean -> a -> Gen Result
propertyIf false _ = pure Success
propertyIf true p  = test p

infix 2 propertyIf as ==>

ordPSQTest :: forall eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
ordPSQTest = do
  log "Test eq instance"
  quickCheck $ \k p v (TestOrdPSQ t) ->
    smallKeyToNumberMap t == smallKeyToNumberMap t && PSQ.insert k p v t == PSQ.insert k p v t
    <?> ("q: " <> show (TestOrdPSQ t))

  log "Test empty"
  do
    let psqEmpty = PSQ.empty :: PSQ.OrdPSQ Int Int Char
    quickCheck (PSQ.size psqEmpty == 0 <?> "invalid size - size should 0 for empty queue")
    quickCheck (PSQ.toAscList psqEmpty == Nil <?> "invalid list convertion for empty queue")

  log "Test findMin"
  do
    let rpsq' = PSQ.fromFoldable [tuple3 5 101 'a', tuple3 3 100 'b']
    quickCheck (PSQ.findMin (PSQ.empty :: PSQ.OrdPSQ Int Int Char) == Nothing <?> "Invalid findMin - empty case")
    quickCheck (PSQ.findMin rpsq' == Just (tuple3 3 100 'b') <?> "Invalid findMin - 3 100 b")

  log "Test build OrdPSQ from Foldable"
  quickCheck \xs ->
    propertyFromFoldable xs
    <?> ("foldable: " <> show xs)

  log "Test size of OrdPSQ"
  quickCheck $ \(TestOrdPSQ t) ->
    PSQ.size (t :: PSQ.OrdPSQ Int Int Char) == length (PSQ.toAscList t)
    <?> ("q: " <> show (TestOrdPSQ t))

  log "Test property deleteMin"
  quickCheck \t ->
    propertyDeleteMin t
    <?> ("q: " <> show t)

  log "Test singleton"
  quickCheck $ \k p v ->
    PSQ.insert k p v PSQ.empty == PSQ.singleton (smallKey k) (number p) (charId v)

  log "Test insert size queue"
  quickCheck $ \k p v t ->
    propertyInsertSize k p v t
    <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v <> ", q: " <> show t)

  log "Test inserting into empty queue"
  quickCheck $ \k p v ->
    PSQ.lookup (smallKey k) (PSQ.insert k p v PSQ.empty) == Just (Tuple (number p) (charId v))
    <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v)

  log "Test member lookup"
  quickCheck $ \k t ->
    propMemberLookup k t
    <?> ("k: " <> show k <> ", queue: " <> show t)

  log "Test lookup delete"
  quickCheck $ \k p v (TestOrdPSQ t) ->
    (PSQ.lookup (smallKey k) t == Nothing) ==>
      (PSQ.delete k (PSQ.insert k p (charId v) t) == t)

  log "Test delete non member"
  quickCheck $ \k (TestOrdPSQ t) ->
    (PSQ.lookup (smallKey k) t == Nothing) ==>
      (PSQ.delete k t == (t :: PSQ.OrdPSQ SmallKey Int Char))

  log "Test Insert deleteView"
  quickCheck $ \k p v t ->
    propertyInsertDeleteView t k p v
    <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v <> ", q: " <> show t)

  log "Test Alter"
  do
    let
      f Nothing                = Tuple "Hello" (Just $ Tuple 100 'a')
      f (Just (Tuple 100 'a')) = Tuple "World" Nothing
      f (Just _)               = Tuple "Cats" (Just $ Tuple 101 'b')
    quickCheck
      (PSQ.alter f 3 (PSQ.empty :: PSQ.OrdPSQ Int Int Char)
        == (Tuple "Hello" (PSQ.singleton 3 100 'a')) <?> "Invalid alter - 3")
    quickCheck
      (PSQ.alter f 3 (PSQ.singleton 3 100 'a') == Tuple "World" PSQ.empty)
    quickCheck
      (PSQ.alter f 3 (PSQ.singleton 3 100 'b') == Tuple "Cats" (PSQ.singleton 3 101 'b'))
