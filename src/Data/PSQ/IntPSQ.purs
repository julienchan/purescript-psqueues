module Data.IntPSQ where

import Prelude

import Data.Int.Bits ((.^.), complement, (.&.), (.|.), zshr)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(..))

import Math (pow)

type Key = Int

data IntPSQ p v
  = Bin Key p v Mask (IntPSQ p v) (IntPSQ p v)
  | Tip Key p v
  | Nil

type ElemRec p v =
  { key   :: Int
  , prio  :: p
  , value :: v
  }

infixr 6 Tuple as /\

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- | Test if Queue is empty
null :: forall p v. IntPSQ p v -> Boolean
null Nil = true
null _   = false

-- | The number of elements in a queue.
size :: forall p v. IntPSQ p v -> Int
size Nil = 0
size (Tip _ _ _) = 1
size (Bin _ _ _ _ l r) = 1 + size l + size r

-- | Test if a key is a member of a queue
member :: forall p v. Key -> IntPSQ p v -> Boolean
member k = isJust <<< lookup k

-- | Get the priority and value of a given key, otherwise Nothing
-- | if is not bound.
lookup :: forall p v. Key -> IntPSQ p v -> Maybe (Tuple p v)
lookup k = go
  where
  go = case _ of
    Nil -> Nothing

    Tip k' p' x'
      | k == k'   -> Just (Tuple p' x')
      | otherwise -> Nothing

    Bin k' p' x' m l r
      | nomatch k k' m  -> Nothing
      | k == k'         -> Just (Tuple p' x')
      | branchLeft m k  -> go l
      | otherwise       -> go r

-- | Get the element with the lowest priority
findMin :: forall p v. IntPSQ p v -> Maybe (ElemRec p v)
findMin Nil = Nothing
findMin (Tip key prio value) = Just { key, prio, value }
findMin (Bin key prio value _ _ _) = Just { key: key, prio, value }

--------------------------------------------------------------------------------
-- Construction ----------------------------------------------------------------
--------------------------------------------------------------------------------

empty :: forall p v. IntPSQ p v
empty = Nil

singleton :: forall p v. Int -> p -> v -> IntPSQ p v
singleton = Tip

--------------------------------------------------------------------------------
-- Insertion -------------------------------------------------------------------
--------------------------------------------------------------------------------

insert :: forall p v. Ord p => Int -> p -> v -> IntPSQ p v -> IntPSQ p v
insert k p x t0 = unsafeInsertNew k p x (delete k t0)

unsafeInsertNew :: forall p v. Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
unsafeInsertNew k p x = go
  where
  go t = case t of
    Nil          -> Tip k p x

    Tip k' p' x'
      | (p /\ k) < (p' /\ k') -> link k p x k' t Nil
      | otherwise             -> link k' p' x' k (Tip k p x) Nil

    Bin k' p' x' m l r
      | nomatch k k' m ->
          if (p /\ k) < (p' /\ k')
            then link k p x k' t Nil
            else link k' p' x' k (Tip k p x) (merge m l r)
      | otherwise ->
          if (p /\ k) < (p' /\ k')
            then
              if branchLeft m k'
                then Bin k p x m (unsafeInsertNew k' p' x' l) r
                else Bin k p x m l (unsafeInsertNew k' p' x' r)
            else
              if branchLeft m k
                then Bin k' p' x' m (unsafeInsertNew k  p  x  l) r
                else Bin k' p' x' m l (unsafeInsertNew k  p  x  r)

link :: forall p v. Key -> p -> v -> Key -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
link k p x k' k't otherTree =
  let m = branchMask k k'
  in if branchLeft m k'
    then Bin k p x m k't otherTree
    else Bin k p x m otherTree k't

--------------------------------------------------------------------------------
-- Delete/Alter ----------------------------------------------------------------
--------------------------------------------------------------------------------

delete :: forall p v. Ord p => Int -> IntPSQ p v -> IntPSQ p v
delete k = go
  where
    go t = case t of
      Nil           -> Nil

      Tip k' _ _
        | k == k'   -> Nil
        | otherwise -> t

      Bin k' p' x' m l r
        | nomatch k k' m  -> t
        | k == k'         -> merge m l r
        | branchLeft m k  -> binShrinkL k' p' x' m (go l) r
        | otherwise       -> binShrinkR k' p' x' m l (go r)

deleteMin :: forall p v. Ord p => IntPSQ p v -> IntPSQ p v
deleteMin t = maybe t _.queue (minView t)

alter
  :: forall b p v
   . Ord p
  => (Maybe (Tuple p v) -> Tuple b (Maybe (Tuple p v)))
  -> Int
  -> IntPSQ p v
  -> Tuple b (IntPSQ p v)
alter f k t0 =
  let t /\ mbX = case deleteView k t0 of
                    Nothing  -> t0 /\ Nothing
                    Just rec -> rec.queue /\ Just (rec.prio /\ rec.value)
  in case f mbX of
    b /\ mbX' -> b /\ maybe t (\(Tuple p v) -> unsafeInsertNew k p v t) mbX'

binShrinkL :: forall p v. Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binShrinkL k p x m Nil r = case r of
  Nil -> Tip k p x
  _   -> Bin k p x m Nil r
binShrinkL k p x m l   r = Bin k p x m l r

binShrinkR :: forall p v. Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binShrinkR k p x m l Nil = case l of
  Nil -> Tip k p x
  _   -> Bin k p x m l Nil
binShrinkR k p x m l r   = Bin k p x m l r

merge :: forall p v. Ord p => Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
merge m l r = case l of
  Nil -> r

  Tip lk lp lx -> case r of
    Nil -> l
    Tip rk rp rx
      | (lp /\ lk) < (rp /\ rk) -> Bin lk lp lx m Nil r
      | otherwise               -> Bin rk rp rx m l Nil
    Bin rk rp rx rm rl rr
       | (lp /\ lk) < (rp /\ rk) -> Bin lk lp lx m Nil r
       | otherwise               -> Bin rk rp rx m l (merge rm rl rr)

  Bin lk lp lx lm ll lr -> case r of
    Nil -> l
    Tip rk rp rx
      | (lp /\ lk) < (rp /\ rk) -> Bin lk lp lx m (merge lm ll lr) r
      | otherwise               -> Bin rk rp rx m l Nil
    Bin rk rp rx rm rl rr
      | (lp /\ lk) < (rp /\ rk) -> Bin lk lp lx m (merge lm ll lr) r
      | otherwise               -> Bin rk rp rx m l (merge rm rl rr)

--------------------------------------------------------------------------------
-- Views -----------------------------------------------------------------------
--------------------------------------------------------------------------------

insertView
  :: forall k p v
   . Ord p
  => Int -> p -> v -> IntPSQ p v -> Tuple (Maybe (Tuple p v)) (IntPSQ p v)
insertView k p x t0 = case deleteView k t0 of
  Nothing  -> Nothing /\ unsafeInsertNew k p x t0
  Just rec -> Just (rec.prio /\ rec.value) /\ unsafeInsertNew k p x rec.queue

deleteView
  :: forall p v. Ord p
  => Int -> IntPSQ p v -> Maybe { prio :: p, value :: v, queue :: IntPSQ p v }
deleteView k t0 = case go t0 of
  _ /\ Nothing       -> Nothing
  t /\ Just (p /\ x) -> Just { prio: p, value: x, queue: t }
  where
  go t = case t of
    Nil -> Nil /\ Nothing

    Tip k' p' x'
      | k == k'   -> Nil /\ Just (p' /\ x')
      | otherwise -> t /\ Nothing

    Bin k' p' x' m l r
      | nomatch k k' m -> t /\ Nothing
      | k == k' ->
          let t' = merge m l r
          in t' /\ Just (p' /\ x')
      | branchLeft m k -> case go l of
          l' /\ mbPX ->
            let t' = binShrinkL k' p' x' m l' r
            in t' /\ mbPX
      | otherwise -> case go r of
          r' /\ mbPX ->
            let t' = binShrinkR k' p' x' m l r'
            in t' /\ mbPX

minView
  :: forall p v. Ord p
  => IntPSQ p v -> Maybe { key :: Key, prio :: p, value :: v, queue :: IntPSQ p v}
minView t = case t of
  Nil       -> Nothing
  Tip k p x -> Just { key: k, prio: p, value: x, queue: Nil }
  Bin k p x m l r -> Just { key: k, prio: p, value: x, queue: merge m l r }

--------------------------------------------------------------------------------
-- Bit Twidling ----------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Mask = Mask Int

unMask :: Mask -> Int
unMask (Mask x) = x

derive newtype instance eqMask :: Eq Mask
derive newtype instance ordMask :: Ord Mask

foreign import dec2bin :: Int -> String
foreign import bin2dec :: String -> Int

maskLonger :: Mask -> Mask -> Boolean
maskLonger m1 m2 = m1 < m2

branchLeft :: Mask -> Int -> Boolean
branchLeft (Mask m) k = m .&. k == 0

nomatch :: Key -> Key -> Mask -> Boolean
nomatch k1 k2 m = mask m k1 /= mask m k2

mask :: Mask -> Int -> Int
mask (Mask m) k = _mask m k

_mask :: Int -> Int -> Int
_mask m k = (k .|. (m-1)) .&. complement m

highestBit :: Int -> Int -> Int
highestBit x m = highb (x .&. complement (m - 1)) m where
  highb :: Int -> Int -> Int
  highb x m
    | x == m = m
    | otherwise = highb (x .&. complement m) (2 * m)

branchingBit' :: Int -> Mask -> Int -> Mask -> Mask
branchingBit' k1 (Mask m1) k2 (Mask m2) =
  Mask (highestBit (k1 .^. k2) (max 1 (2 * max m1 m2)))

branchingBit :: Int -> Int -> Mask
branchingBit k1 k2 = branchingBit' k1 (Mask 0) k2 (Mask 0)

branchMask :: Int -> Int -> Mask
branchMask x1 x2 =
  Mask (highestBitMask (x1 .^. x2))

highestBitMask :: Int -> Int
highestBitMask x1 =
  let x2 = x1 .|. x1 `zshr` 1
      x3 = x2 .|. x2 `zshr` 2
      x4 = x3 .|. x3 `zshr` 4
      x5 = x4 .|. x4 `zshr` 8
      x6 = x5 .|. x5 `zshr` 16
  in x6 .^. (x6 `zshr` 1)
