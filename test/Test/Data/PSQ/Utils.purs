module Test.Data.PSQ.Utils where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldr)
import Data.List (List(Nil), (:), uncons)
import Data.NonEmpty ((:|))
import Data.Unfoldable (replicateA)

import Data.OrdPSQ as OrdPSQ
import Data.IntPSQ as IntPSQ
import Test.Unit (TestSuite)
import Test.QuickCheck (class Testable, Result(Success), test)
import Test.QuickCheck.Gen (Gen, frequency, chooseInt, elements)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)


propertyIf :: forall a. Testable a => Boolean -> a -> Gen Result
propertyIf false _ = pure Success
propertyIf true p  = test p

infix 2 propertyIf as ==>

type PSQTestSuite eff = TestSuite
  ( console :: CONSOLE
  , random :: RANDOM
  , exception :: EXCEPTION
  | eff
  )

type ElemRec k p v =
    { key   :: k
    , prio  :: p
    , value :: v
    }

class PSQ psq k | psq -> k where
    null :: forall p v. Ord p => psq p v -> Boolean
    size :: forall p v. Ord p => psq p v -> Int
    member :: forall p v. Ord p => k -> psq p v -> Boolean
    lookup :: forall p v. Ord p => k -> psq p v -> Maybe (Tuple p v)
    findMin :: forall p v. Ord p => psq p v -> Maybe (ElemRec k p v)
    empty :: forall p v. Ord p => psq p v
    singleton :: forall p v. Ord p => k -> p -> v -> psq p v
    insert :: forall p v. Ord p => k -> p -> v -> psq p v -> psq p v
    delete :: forall p v. Ord p => k -> psq p v -> psq p v
    deleteMin :: forall p v. Ord p => psq p v -> psq p v
    alter :: forall b p v. Ord p => (Maybe (Tuple p v) -> Tuple b (Maybe (Tuple p v)))
                                 -> k -> psq p v -> Tuple b (psq p v)
    alterMin :: forall b p v. Ord p => (Maybe (ElemRec k p v) -> Tuple b (Maybe (ElemRec k p v)))
                                    -> psq p v -> Tuple b (psq p v)
    fromFoldable :: forall f p v. Ord p => Foldable f => f (ElemRec k p v) -> psq p v
    toList :: forall p v. psq p v -> List (ElemRec k p v)
    keys :: forall p v. Ord p => psq p v -> List k
    insertView :: forall p v. Ord p => k -> p -> v -> psq p v -> Tuple (Maybe (Tuple p v)) (psq p v)
    deleteView :: forall p v. Ord p => k -> psq p v -> Maybe { prio :: p, value :: v, queue :: psq p v }
    minView :: forall p v. Ord p => psq p v -> Maybe { key :: k, prio :: p, value :: v, queue :: psq p v}

instance psqOrdPSQ :: Ord k => PSQ (OrdPSQ.OrdPSQ k) k where
    null = OrdPSQ.null
    size = OrdPSQ.size
    member = OrdPSQ.member
    lookup = OrdPSQ.lookup
    findMin = OrdPSQ.findMin
    empty = OrdPSQ.empty
    singleton = OrdPSQ.singleton
    insert = OrdPSQ.insert
    delete = OrdPSQ.delete
    deleteMin = OrdPSQ.deleteMin
    alter = OrdPSQ.alter
    alterMin = OrdPSQ.alterMin
    fromFoldable = OrdPSQ.fromFoldable
    toList = OrdPSQ.toAscList
    keys = OrdPSQ.keys
    insertView = OrdPSQ.insertView
    deleteView = OrdPSQ.deleteView
    minView = OrdPSQ.minView

instance psqIntPSQ :: PSQ IntPSQ.IntPSQ Int where
    null = IntPSQ.null
    size = IntPSQ.size
    member = IntPSQ.member
    lookup = IntPSQ.lookup
    findMin = IntPSQ.findMin
    empty = IntPSQ.empty
    singleton = IntPSQ.singleton
    insert = IntPSQ.insert
    delete = IntPSQ.delete
    deleteMin = IntPSQ.deleteMin
    alter = IntPSQ.alter
    alterMin = IntPSQ.alterMin
    fromFoldable = IntPSQ.fromFoldable
    toList = IntPSQ.toList
    keys = IntPSQ.keys
    insertView = IntPSQ.insertView
    deleteView = IntPSQ.deleteView
    minView = IntPSQ.minView

data Action k p v
    = Insert k p v
    | DeleteRandomMember
    | DeleteMin

derive instance eqAction :: (Eq k, Eq p, Eq v) => Eq (Action k p v)

instance showAction :: (Show k, Show p, Show v) => Show (Action k p v) where
    show (Insert k p v) = "(Insert " <> show k <> " " <> show p <> " " <> show v <> " )"
    show DeleteRandomMember = "DeleteRandomMember"
    show DeleteMin = "DeleteMin"

arbitraryPriority :: Gen Int
arbitraryPriority = chooseInt (-10) 10

applyAction
    :: forall psq k v. PSQ psq k => Action k Int v -> psq Int v -> Gen (psq Int v)
applyAction (Insert k p v) psq      = pure $ insert k p v psq
applyAction DeleteRandomMember psq  = do
    let ks = keys psq
    case uncons ks of
        Nothing             -> pure psq
        Just { head, tail } -> do
            key <- elements (head :| A.fromFoldable tail)
            pure $ delete key psq
applyAction DeleteMin psq = pure $ case minView psq of
    Nothing        -> psq
    Just { queue } -> queue

arbitraryPSQ
    :: forall psq k v. PSQ psq k => Arbitrary k => Arbitrary v
    => Gen (psq Int v)
arbitraryPSQ = do
    numActions <- chooseInt 0 100
    actions <- replicateA numActions (arbitrary :: Gen (Action k Int v))
    foldLM (\t a -> applyAction a t) (empty :: psq Int v) actions
    where
      foldLM :: forall a b m. Monad m => (b -> a -> m b) -> b -> List a -> m b
      foldLM f z0 xs = foldr f' pure xs z0
        where
          f' x k z = f z x >>= k

newtype TestOrdPSQ k v = TestOrdPSQ (OrdPSQ.OrdPSQ k Int v)

newtype TestIntPSQ v   = TestIntPSQ (IntPSQ.IntPSQ Int v)

instance arbitraryAction :: (Arbitrary k, Arbitrary v) => Arbitrary (Action k Int v) where
    arbitrary = frequency $
        Tuple 10.0 (Insert <$> arbitrary <*> arbitraryPriority <*> arbitrary)
        :|
        ( Tuple 2.0 (pure DeleteRandomMember)
        : Tuple 2.0 (pure DeleteMin)
        : Nil)

instance arbitraryTestOrdPSQ :: (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (TestOrdPSQ k v) where
    arbitrary = TestOrdPSQ <$> arbitraryPSQ

instance arbitraryIntPSQ :: Arbitrary v => Arbitrary (TestIntPSQ v) where
    arbitrary = TestIntPSQ <$> arbitraryPSQ

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
