module Test.Data.PSQ
  ( psqTest
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.Foldable (foldr, all)
import Data.List (List(Nil), (:), (!!), uncons, reverse, length)
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.NonEmpty ((:|))
import Data.Unfoldable (replicateA)

import Partial.Unsafe (unsafePartial)

import Test.QuickCheck ((<?>), quickCheck, class Testable, Result(Success), test)
import Test.QuickCheck.Gen (Gen, frequency, chooseInt, elements)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Data.PSQ.Internal as PSQ

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

applyAction :: forall k v. Ord k => Action k v -> PSQ.PSQ k Int v -> Gen (PSQ.PSQ k Int v)
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
  Just { queue } -> queue

instance arbitraryAction :: (Arbitrary p, Arbitrary v) => Arbitrary (Action p v) where
  arbitrary = frequency $
    Tuple 10.0 (Insert <$> arbitrary <*> arbitraryPriority <*> arbitrary)
    :|
    (Tuple 2.0 (pure DeleteRandomMember)
    : Tuple 2.0 (pure DeleteMin)
    : Nil)

newtype TestPSQ k v = TestPSQ (PSQ.PSQ k Int v)

instance arbTestPSQ :: (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (TestPSQ k v) where
  arbitrary = do
    numActions <- chooseInt 0 100
    actions <- replicateA numActions (arbitrary :: Gen (Action k v))
    TestPSQ <$> foldLM (\t a -> applyAction a t) PSQ.empty actions
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

smallKeyToNumberMap :: PSQ.PSQ SmallKey Int String -> PSQ.PSQ SmallKey Int String
smallKeyToNumberMap m = m

propMemberLookup :: SmallKey -> PSQ.PSQ SmallKey Int String -> Boolean
propMemberLookup k t = case PSQ.lookup k t of
  Nothing -> not (PSQ.member k t)
  Just _  -> PSQ.member k t

propertyInsertSize :: SmallKey -> Int -> String -> PSQ.PSQ SmallKey Int String -> Boolean
propertyInsertSize k p s t =
  let t' = PSQ.insert k p s t
  in if PSQ.member k t then PSQ.size t == PSQ.size t' else PSQ.size t' == (PSQ.size t) + 1

propertyDeleteMin :: PSQ.PSQ SmallKey Int Char -> Boolean
propertyDeleteMin t =
  let
    t' = PSQ.deleteMin t
  in
  if PSQ.null t
    then t == t'
    else case PSQ.findMin t of
           Nothing -> false
           Just rec ->
             PSQ.size t' == PSQ.size t - 1
             && PSQ.member rec.key t
             && not (PSQ.member rec.key t')

propertyFromFoldable :: List (Tuple Int (Tuple Int Char)) -> Boolean
propertyFromFoldable xs =
  PSQ.fromFoldable (fromTuple <$> xs) == PSQ.fromFoldable (fromTuple <$> reverse xs)
  where
    fromTuple :: Tuple Int (Tuple Int Char) -> PSQ.ElemRec Int Int Char
    fromTuple (Tuple k (Tuple p v)) = PSQ.elemRec k p v

propertyInsertDeleteView :: PSQ.PSQ SmallKey Int Char -> SmallKey -> Int -> Char -> Boolean
propertyInsertDeleteView t k p c =
  case PSQ.deleteView k (PSQ.insert k p c t) of
    Nothing -> false
    Just rec
      | PSQ.member k t -> rec.prio == p && rec.value == c && PSQ.size rec.queue < PSQ.size t
      | otherwise      -> rec.prio == p && rec.value == c && rec.queue == t

propertyAlter :: PSQ.PSQ SmallKey Int Char -> SmallKey -> Boolean
propertyAlter t k =
  let Tuple _ t' = PSQ.alter f k t
  in case PSQ.lookup k t of
    Just _    -> (PSQ.size t - 1) == PSQ.size t' && PSQ.lookup k t' == Nothing
    Nothing   -> (PSQ.size t + 1) == PSQ.size t' && PSQ.lookup k t' /= Nothing
  where
    f Nothing   = Tuple unit $ Just (Tuple 100 'a')
    f (Just _)  = Tuple unit Nothing

propertyIf :: forall a. Testable a => Boolean -> a -> Gen Result
propertyIf false _ = pure Success
propertyIf true p  = test p

elemEq :: forall k p v. Eq k => Eq p => Eq v => PSQ.ElemRec k p v -> PSQ.ElemRec k p v -> Boolean
elemEq x y = x.key == y.key && x.prio == y.prio && x.value == y.value

infix 2 propertyIf as ==>

psqTest :: forall eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
psqTest = do
  log "Test eq instance"
  quickCheck $ \k p v (TestPSQ t) ->
    smallKeyToNumberMap t == smallKeyToNumberMap t && PSQ.insert k p v t == PSQ.insert k p v t
    <?> ("q: " <> show t)

  log "Test build PSQ from Foldable"
  quickCheck \xs ->
    propertyFromFoldable xs
    <?> ("foldable: " <> show xs)

  log "Test size of PSQ"
  quickCheck $ \(TestPSQ t) ->
    PSQ.size (t :: PSQ.PSQ Int Int Char) == length (PSQ.toAscList t)
    <?> ("q: " <> show t)

  log "Test property deleteMin"
  quickCheck \(TestPSQ t) ->
    propertyDeleteMin t
    <?> ("q: " <> show t)

  log "Test singleton"
  quickCheck $ \k p v ->
    PSQ.insert k p v PSQ.empty == PSQ.singleton (smallKey k) (number p) (charId v)

  log "Test insert size queue"
  quickCheck $ \k p v (TestPSQ t) ->
    propertyInsertSize k p v t
    <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v <> ", q: " <> show t)

  log "Test inserting into empty queue"
  quickCheck $ \k p v ->
    PSQ.lookup (smallKey k) (PSQ.insert k p v PSQ.empty) == Just (Tuple (number p) (charId v))
    <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v)

  log "Test member lookup"
  quickCheck $ \k (TestPSQ t) ->
    propMemberLookup k t
    <?> ("k: " <> show k <> ", queue: " <> show t)

  log "Test lookup delete"
  quickCheck $ \k p v (TestPSQ t) ->
    (PSQ.lookup (smallKey k) t == Nothing) ==>
      (PSQ.delete k (PSQ.insert k p (charId v) t) == t)

  log "Test delete non member"
  quickCheck $ \k (TestPSQ t) ->
    (PSQ.lookup (smallKey k) t == Nothing) ==>
      (PSQ.delete k t == (t :: PSQ.PSQ SmallKey Int Char))

  log "Test Insert deleteView"
  quickCheck $ \k p v (TestPSQ t) ->
    propertyInsertDeleteView t k p v
    <?> ("k: " <> show k <> ", p: " <> show p <> ", v: " <> show v <> ", q: " <> show t)

  log "Test property alter"
  quickCheck $ \k (TestPSQ t) ->
    propertyAlter t k
    <?> ("k: " <> show k <> ", q: " <> show t)

  log "Test atMost - ascending keys"
  quickCheck $ \(TestPSQ t) ->
    let elList        = PSQ.atMost 6 t
        testAsc xs =
          let x = unsafePartial $ fromJust $ xs !! 0
              y = unsafePartial $ fromJust $ xs !! 1
          in x.key <= y.key
        testAsc' :: List (PSQ.ElemRec Int Int Char) -> Unit -> Boolean
        testAsc' xs _ = testAsc xs
    -- | all items should be in order of ascending keys
    in (length elList) >= 2 ==> testAsc' elList

  log "Test atMost - priority"
  quickCheck $ \(TestPSQ t) ->
    let el = PSQ.atMost 5 t :: List (PSQ.ElemRec Int Int Char)
    in all ((>=) 5 <<< _.prio) el

  log "Test empty"
  do
    let psqEmpty = PSQ.empty :: PSQ.PSQ Int Int Char
    quickCheck (PSQ.size psqEmpty == 0 <?> "invalid size - size should 0 for empty queue")

  log "Test findMin"
  do
    let rpsq' = PSQ.fromFoldable [PSQ.elemRec 5 101 'a', PSQ.elemRec 3 100 'b']
    quickCheck (isNothing (PSQ.findMin (PSQ.empty :: PSQ.PSQ Int Int Char)) <?> "Invalid findMin - empty case")
    quickCheck ((elemEq (PSQ.elemRec 3 100 'b') <$> PSQ.findMin rpsq') == Just true <?> "Invalid findMin - 3 100 b")

  log "Test Alter"
  do
    let
      f Nothing                = Tuple "Hello" (Just $ Tuple 100 'a')
      f (Just (Tuple 100 'a')) = Tuple "World" Nothing
      f (Just _)               = Tuple "Cats" (Just $ Tuple 101 'b')
    quickCheck
      (PSQ.alter f 3 (PSQ.empty :: PSQ.PSQ Int Int Char)
        == (Tuple "Hello" (PSQ.singleton 3 100 'a')) <?> "Invalid alter - 3")
    quickCheck
      (PSQ.alter f 3 (PSQ.singleton 3 100 'a') == Tuple "World" PSQ.empty)
    quickCheck
      (PSQ.alter f 3 (PSQ.singleton 3 100 'b') == Tuple "Cats" (PSQ.singleton 3 101 'b'))
