module Data.OrdPSQ.Internal
  ( Elem(..)
  , OrdPSQ(..)
  , LTree(..)
  , Size
  -- query
  , null
  , size
  , member
  , lookup
  , findMin
  -- construction
  , empty
  , singleton
  -- Insertion
  , insert
   -- Delete/Update
  , delete
  , deleteMin
  , alter
  , alterMin
  -- Convertion
  , fromFoldable
  , toAscList
  , keys
  -- Views
  , insertView
  , deleteView
  , minView
  -- * Tournament view
  , TourView(..)
  , tourView
  , play
  -- Balancing internals
  , left
  , right
  , maxKey
  , lsingleLeft
  , rsingleLeft
  , lsingleRight
  , rsingleRight
  , ldoubleLeft
  , rdoubleLeft
  , ldoubleRight
  , rdoubleRight) where

import Prelude hiding (Void)

import Data.Foldable (class Foldable, foldr)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\), tuple3, tuple4, Tuple3, Tuple4)
import Partial.Unsafe (unsafePartial)

-- | `Elem k p v ` respresent an element on Queue. It's mean key `k` with value `v`
-- | and priority `p`.
data Elem k p v = Elem k p v

instance functorElem :: Functor (Elem k p) where
  map f (Elem k p v) = Elem k p (f v)

-- | A mapping from keys `k` to priorites `p` and values `v`.
data OrdPSQ k p v
  = Void
  | Winner (Elem k p v) (LTree k p v) k

instance functorOrdPSQ :: Functor (OrdPSQ k p) where
  map _ Void = Void
  map f (Winner el lt k) = Winner (f <$> el) (f <$> lt) k

instance eqOrdPSQ :: (Ord k, Ord p, Eq v) => Eq (OrdPSQ k p v) where
  eq x y = case minView x, minView y of
    Nothing, Nothing -> true
    Just (xk /\ xp /\ xv /\ x' /\ _), Just (yk /\ yp /\ yv /\ y' /\ _) ->
      xk == yk && xp == yp && xv == yv && x' == y'
    Just _, Nothing -> false
    Nothing, Just _ -> false

instance showOrdPSQ :: (Show k, Show p, Show v) => Show (OrdPSQ p k v) where
  show t = "(OrdPSQ " <> show (toAscList t) <> " )"

type Size = Int

data LTree k p v
  = Start
  | LLoser Size (Elem k p v) (LTree k p v) k (LTree k p v)
  | RLoser Size (Elem k p v) (LTree k p v) k (LTree k p v)

instance functorLtree :: Functor (LTree k p) where
  map _ Start = Start
  map f (LLoser s el lt k rt) = LLoser s (f <$> el) (f <$> lt) k (f <$> rt)
  map f (RLoser s el lt k rt) = RLoser s (f <$> el) (f <$> lt) k (f <$> rt)

-- | Test if Queue is empty
null :: forall k p v. OrdPSQ k p v -> Boolean
null Void = true
null _    = false

-- | The number of elements in a queue.
size :: forall k p v. OrdPSQ k p v -> Int
size Void            = 0
size (Winner _ lt _) = 1 + size' lt

-- | Test if a key is a member of a queue
member :: forall k p v. Ord k => k -> OrdPSQ k p v -> Boolean
member k = isJust <<< lookup k

-- | Get the priority and value of a given key, otherwise Nothing
-- | if is not bound.
-- |
-- | Running time: `O(log n)`
lookup :: forall k p v. Ord k => k -> OrdPSQ k p v -> Maybe (Tuple p v)
lookup k = go
  where
    go t = case tourView t of
      Null                  -> Nothing
      Single (Elem k' p v)
        | k == k'           -> Just (Tuple p v)
        | otherwise         -> Nothing
      Play tl tr            -> if k <= unsafePartial (maxKey tl) then go tl else go tr

-- | Get the element with the lowest priority
-- |
-- | Running time: `O(1)`
findMin :: forall k p v. OrdPSQ k p v -> Maybe (Tuple3 k p v)
findMin Void                      = Nothing
findMin (Winner (Elem k p v) _ _) = Just $ tuple3 k p v

-- | Construct an empty queue
empty :: forall k p v. OrdPSQ k p v
empty = Void

-- | Create a queue with a single element.
-- |
-- | Running time: `O(1)``
singleton :: forall k p v. k -> p -> v -> OrdPSQ k p v
singleton k p v = Winner (Elem k p v) Start k

-- | Insert a new key, priority and value into the queue. If the key is
-- | already present in the queue, the associated priority and value are replaced
-- | with the supplied priority and value.
-- |
-- | Running time: `O(log n)`
insert :: forall k p v. Ord k => Ord p => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insert k p v = go
  where
    go t = case t of
      Void -> singleton k p v
      Winner (Elem k' p' v') Start _ -> case compare k k' of
        LT -> singleton k p v `play` singleton k' p' v'
        EQ -> singleton k p v
        GT -> singleton k' p' v' `play` singleton k p v
      Winner e (RLoser _ e' tl m tr) m'
        | k <= m      -> go (Winner e tl m) `play` (Winner e' tr m')
        | otherwise   -> (Winner e tl m) `play` go (Winner e' tr m')
      Winner e (LLoser _ e' tl m tr) m'
        | k <= m      -> go (Winner e' tl m) `play` (Winner e tr m')
        | otherwise   -> (Winner e' tl m) `play` go (Winner e tr m')

-- | Delete a key and its priority and value from the queue. When the
-- | key is not a member of the queue, the original queue is returned.
-- |
-- | Running time: `O(log n)`
delete :: forall k p v. Ord k => Ord p => k -> OrdPSQ k p v -> OrdPSQ k p v
delete k = go
  where
    go t = case t of
      Void -> empty
      Winner (Elem k' p v) Start _
        | k == k'   -> empty
        | otherwise -> singleton k' p v
      Winner e (RLoser _ e' tl m tr) m'
        | k <= m    -> go (Winner e tl m) `play` (Winner e' tr m')
        | otherwise -> (Winner e tl m) `play` go (Winner e' tr m')
      Winner e (LLoser _ e' tl m tr) m'
        | k <= m    -> go (Winner e' tl m) `play` (Winner e tr m')
        | otherwise -> (Winner e' tl m) `play` go (Winner e tr m')

deleteMin :: forall k p v. Ord k => Ord p => OrdPSQ k p v -> OrdPSQ k p v
deleteMin t = case minView t of
  Nothing                       -> t
  Just (_ /\ _ /\ _ /\ t' /\ _) -> t'

alter
  :: forall b k p v
   . Ord k => Ord p
  => (Maybe (Tuple p v) -> Tuple b (Maybe (Tuple p v)))
  -> k
  -> OrdPSQ k p v
  -> Tuple b (OrdPSQ k p v)
alter f k psq0 =
  let psq1 /\ mbPV = case deleteView k psq0 of
                          Nothing                   -> psq0 /\ Nothing
                          Just (p /\ v /\ psq /\ _) -> psq /\ Just (p /\ v)
      b /\ mbPV'   = f mbPV
  in case mbPV' of
    Nothing          -> b /\ psq1
    Just (p /\ v)    -> b /\ (insert k p v psq1)

alterMin
  :: forall b k p v
   . Ord k => Ord p
  => (Maybe (Tuple3 k p v) -> Tuple b (Maybe (Tuple3 k p v)))
  -> OrdPSQ k p v
  -> Tuple b (OrdPSQ k p v)
alterMin f psq0 = case minView psq0 of
  Nothing ->
    let b /\ mbKPV = f Nothing
    in b /\ insertMay mbKPV psq0
  Just (k /\ p /\ v /\ psq1 /\ _) ->
    let b /\ mbKPV = f $ Just (tuple3 k p v)
    in b /\ insertMay mbKPV psq1
  where
    insertMay Nothing                   psq = psq
    insertMay (Just (k /\ p /\ v /\ _)) psq = insert k p v psq

fromFoldable
  :: forall f k p v
   . Foldable f => Ord k => Ord p
  => f (Tuple3 k p v)
  -> OrdPSQ k p v
fromFoldable = foldr accum empty
  where
    accum (k /\ p /\ v /\ _) q = insert k p v q

keys :: forall k p v. OrdPSQ k p v -> List k
keys = map fst <<< toAscList

toAscList :: forall k p v. OrdPSQ k p v -> List (Tuple3 k p v)
toAscList q = go q
  where
  go t = case tourView t of
    Null                -> Nil
    Single (Elem k p v) -> (tuple3 k p v : Nil)
    Play tl tr          -> go tl <> go tr

insertView
  :: forall k p v
   . Ord k => Ord p
  => k -> p -> v -> OrdPSQ k p v -> Tuple (Maybe (Tuple p v)) (OrdPSQ k p v)
insertView k p x t = case deleteView k t of
  Nothing                   -> Nothing /\ insert k p x t
  Just (p' /\ x' /\ _ /\ _) -> Just (p' /\ x') /\ insert k p x t

deleteView
  :: forall k p v
   . Ord k => Ord p => k -> OrdPSQ k p v -> Maybe (Tuple3 p v (OrdPSQ k p v))
deleteView k psq = case psq of
  Void            -> Nothing
  Winner (Elem k' p v) Start _
      | k == k'   -> Just $ tuple3 p v empty
      | otherwise -> Nothing
  Winner e (RLoser _ e' tl m tr) m'
      | k <= m    -> map (\(p /\ v /\ q /\ _) -> tuple3 p v $ q `play` (Winner e' tr m')) (deleteView k (Winner e tl m))
      | otherwise -> map (\(p /\ v /\ q /\ _) -> tuple3 p v $ (Winner e tl m) `play` q ) (deleteView k (Winner e' tr m'))
  Winner e (LLoser _ e' tl m tr) m'
      | k <= m    -> map (\(p /\ v /\ q /\ _) -> tuple3 p v $ q `play` (Winner e tr m')) (deleteView k (Winner e' tl m))
      | otherwise -> map (\(p /\ v /\ q /\ _) -> tuple3 p v $ (Winner e' tl m) `play` q) (deleteView k (Winner e tr m'))

minView :: forall k p v. Ord k => Ord p => OrdPSQ k p v -> Maybe (Tuple4 k p v (OrdPSQ k p v))
minView Void                      = Nothing
minView (Winner (Elem k p v) t m) = Just $ tuple4 k p v (secondBest t m)

secondBest :: forall k p v. Ord k => Ord p => LTree k p v -> k -> OrdPSQ k p v
secondBest Start _                 = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'

data TourView k p v
  = Null
  | Single (Elem k p v)
  | Play (OrdPSQ k p v) (OrdPSQ k p v)

tourView :: forall k p v. OrdPSQ k p v -> TourView k p v
tourView Void                                = Null
tourView (Winner e Start _)                  = Single e
tourView (Winner e (RLoser _ e' tl m tr) m') = Winner e tl m `Play` Winner e' tr m'
tourView (Winner e (LLoser _ e' tl m tr) m') = Winner e' tl m `Play` Winner e tr m'

play :: forall k p v. Ord k => Ord p => OrdPSQ k p v -> OrdPSQ k p v -> OrdPSQ k p v
play Void t' = t'
play t Void  = t
play (Winner e@(Elem k p v) t m) (Winner e'@(Elem k' p' v') t' m')
  | Tuple p k `beats` Tuple p' k' = Winner e (unsafePartial $ rbalance k' p' v' t m t') m'
  | otherwise                     = Winner e' (unsafePartial $ lbalance k p v t m t') m'

beats :: forall p k. Ord p => Ord k => Tuple p k -> Tuple p k -> Boolean
beats (p /\ k) (p' /\ k') = p < p' || (p == p' && k < k')

omega :: Int
omega = 4

size' :: forall k p v. LTree k p v -> Size
size' Start              = 0
size' (LLoser s _ _ _ _) = s
size' (RLoser s _ _ _ _) = s

left :: forall k p v. Partial => LTree k p v -> LTree k p v
left (LLoser _ _ tl _ _ ) = tl
left (RLoser _ _ tl _ _ ) = tl

right :: forall k p v. Partial => LTree k p v -> LTree k p v
right (LLoser _ _ _  _ tr) = tr
right (RLoser _ _ _  _ tr) = tr

maxKey :: forall k p v. Partial => OrdPSQ k p v -> k
maxKey (Winner _ _ m) = m

lloser :: forall k p v. k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lloser k p v tl m tr = LLoser (1 + size' tl + size' tr) (Elem k p v) tl m tr

rloser :: forall k p v. k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rloser k p v tl m tr = RLoser (1 + size' tl + size' tr) (Elem k p v) tl m tr

lbalance
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalance k p v l m r
  | size' l + size' r < 2     = lloser        k p v l m r
  | size' r > omega * size' l = lbalanceLeft  k p v l m r
  | size' l > omega * size' r = lbalanceRight k p v l m r
  | otherwise                 = lloser        k p v l m r

rbalance
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalance k p v l m r
  | size' l + size' r < 2     = rloser        k p v l m r
  | size' r > omega * size' l = rbalanceLeft  k p v l m r
  | size' l > omega * size' r = rbalanceRight k p v l m r
  | otherwise                 = rloser        k p v l m r

lbalanceLeft
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceLeft  k p v l m r
  | size' (left r) < size' (right r) = lsingleLeft  k p v l m r
  | otherwise                        = ldoubleLeft  k p v l m r

lbalanceRight
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceRight k p v l m r
  | size' (left l) > size' (right l) = lsingleRight k p v l m r
  | otherwise                        = ldoubleRight k p v l m r

rbalanceLeft
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceLeft  k p v l m r
  | size' (left r) < size' (right r) = rsingleLeft  k p v l m r
  | otherwise                        = rdoubleLeft  k p v l m r

rbalanceRight
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceRight k p v l m r
  | size' (left l) > size' (right l) = rsingleRight k p v l m r
  | otherwise                        = rdoubleRight k p v l m r

lsingleLeft
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3)
  | Tuple p1 k1 `beats` Tuple p2 k2 =
      lloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
  | otherwise                 =
      lloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
lsingleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
  rloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3

rsingleLeft
  :: forall k p v
   . Partial => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
rsingleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (rloser k1 p1 v1 t1 m1 t2) m2 t3

lsingleRight
  :: forall k p v
   . Partial => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (lloser k1 p1 v1 t2 m2 t3)
lsingleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)

rsingleRight
    :: forall k p v
     . Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3
    | Tuple p1 k1 `beats` Tuple p2 k2 =
        rloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
    | otherwise                 =
        rloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)

ldoubleLeft
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3) =
  lsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
  lsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)

ldoubleRight
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
  lsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
  lsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3

rdoubleLeft
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3) =
  rsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
  rsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)

rdoubleRight
  :: forall k p v
   . Partial => Ord k => Ord p
  => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
  rsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
  rsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
