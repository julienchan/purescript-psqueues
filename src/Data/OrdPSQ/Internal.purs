module Data.OrdPSQ.Internal
    ( Elem(..)
    , ElemRec
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
    , elemRec
    -- Insertion
    , insert
    -- Delete/Update
    , delete
    , deleteMin
    , adjust
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
    -- Traversals
    , mapWithKeyPrio
    -- * Tournament view
    , TourView(..)
    , tourView
    , play
    , unsafePlay
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

import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Data.Traversable (class Traversable, traverse)
import Partial.Unsafe (unsafePartial)

-- | An element with key `k`, value `v` and priority `p`.
data Elem k p v = Elem k p v

type ElemRec k p v =
    { key   :: k
    , prio  :: p
    , value :: v
    }

instance functorElem :: Functor (Elem k p) where
    map f (Elem k p v) = Elem k p (f v)

-- | A mapping from keys `k` to priorites `p` and values `v`.
data OrdPSQ k p v
    = Void
    | Winner (Elem k p v) (LTree k p v) k

instance functorPSQ :: Functor (OrdPSQ k p) where
    map _ Void = Void
    map f (Winner el lt k) = Winner (f <$> el) (f <$> lt) k

instance foldablePSQ :: Foldable (OrdPSQ k p) where
    foldl   f z q = foldl f z (_.value <$> toAscList q)
    foldr   f z q = foldr f z (_.value <$> toAscList q)
    foldMap f   q = foldMap f (_.value <$> toAscList q)

instance traversablePSQ :: Traversable (OrdPSQ k p) where
    traverse _ Void             = pure Void
    traverse f (Winner el lt m) = Winner
        <$> goElem el
        <*> goTree lt
        <*> pure m
      where
        goElem (Elem k p v) = Elem k p <$> f v

        goTree Start = pure Start
        goTree (LLoser s el' lt' m' rt') = LLoser s
            <$> goElem el'
            <*> goTree lt'
            <*> pure m'
            <*> goTree rt'
        goTree (RLoser s el' lt' m' rt') = RLoser s
            <$> goElem el'
            <*> goTree lt'
            <*> pure m'
            <*> goTree rt'
    sequence = traverse id

instance eqPSQ :: (Ord k, Ord p, Eq v) => Eq (OrdPSQ k p v) where
    eq x y = case minView x, minView y of
        Nothing, Nothing     -> true
        Just xx, Just yy ->
            xx.key == yy.key && xx.value == yy.value && xx.prio == yy.prio && xx.queue == yy.queue
        Just _, Nothing -> false
        Nothing, Just _ -> false

instance showPSQ :: (Show k, Show p, Show v) => Show (OrdPSQ p k v) where
    show t = "(OrdPSQ " <> show (showElem <$> toAscList t) <> " )"
      where
        showElem rec =
            "{ key: " <> show rec.key
            <> ", prio: " <> show rec.prio
            <> ", value: " <> show rec.value
            <> " }"

-- | The size of LTree
type Size = Int

data LTree k p v
    = Start
    | LLoser Size (Elem k p v) (LTree k p v) k (LTree k p v)
    | RLoser Size (Elem k p v) (LTree k p v) k (LTree k p v)

instance functorLtree :: Functor (LTree k p) where
    map _ Start = Start
    map f (LLoser s el lt k rt) = LLoser s (f <$> el) (f <$> lt) k (f <$> rt)
    map f (RLoser s el lt k rt) = RLoser s (f <$> el) (f <$> lt) k (f <$> rt)

infixr 6 Tuple as /\

-- | Test if Queue is empty
null :: forall k p v. OrdPSQ k p v -> Boolean
null Void = true
null _    = false

-- | The number of elements in a queue.
size :: forall k p v. OrdPSQ k p v -> Int
size Void            = 0
size (Winner _ lt _) = 1 + size' lt

-- | Test if a key is a member of a queue
-- |
-- | Running time: `O(log n)`
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
            | k == k'         -> Just (Tuple p v)
            | otherwise       -> Nothing
        Play tl tr            -> if k <= unsafePartial (maxKey tl) then go tl else go tr

-- | Get the element with the lowest priority
-- |
-- | Running time: `O(1)`
findMin :: forall k p v. OrdPSQ k p v -> Maybe (ElemRec k p v)
findMin Void                      = Nothing
findMin (Winner (Elem k p v) _ _) = Just { key: k, prio: p, value: v }

-- | Construct an empty queue
empty :: forall k p v. OrdPSQ k p v
empty = Void

-- | Create a queue with a single element.
-- |
-- | Running time: `O(1)``
singleton :: forall k p v. k -> p -> v -> OrdPSQ k p v
singleton k p v = Winner (Elem k p v) Start k

-- | Create an element record, useful when building PSQ from `Foldable`
-- |
elemRec :: forall k p v. k -> p -> v -> ElemRec k p v
elemRec = { key: _, prio: _, value: _ }

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

-- | Update a priority at a specific key with the result of the provided function.
-- | When the key is not a member of the queue, the original queue is returned.
-- |
-- | Running time: `O(log n)`
adjust :: forall k p v. Ord k => Ord p => (p -> p) -> k -> OrdPSQ k p v -> OrdPSQ k p v
adjust f k = go
  where
    go t = case t of
        Void -> empty
        Winner (Elem k' p v) Start _
            | k == k'   -> singleton k' (f p) v
            | otherwise -> singleton k' p v
        Winner e (RLoser _ e' tl m tr) m'
            | k <= m    -> go (Winner e tl m) `unsafePlay` (Winner e' tr m')
            | otherwise -> (Winner e tl m) `unsafePlay` go (Winner e' tr m')
        Winner e (LLoser _ e' tl m tr) m'
            | k <= m    -> go (Winner e' tl m) `unsafePlay` (Winner e tr m')
            | otherwise -> (Winner e' tl m) `unsafePlay` go (Winner e tr m')

-- | Delete an item with the least priority, and return the rest of the queue.
-- | In case the queue is empty, the original queue returned
-- |
-- | Running time: `O(log n)`
deleteMin :: forall k p v. Ord k => Ord p => OrdPSQ k p v -> OrdPSQ k p v
deleteMin t = case minView t of
    Nothing        -> t
    Just { queue } -> queue

-- | This function can be used to update, delete, and insert a value and its priority
-- | for the given key.
-- |
-- | Running time: `O(log n)`
alter
    :: forall b k p v . Ord k => Ord p
    => (Maybe (Tuple p v) -> Tuple b (Maybe (Tuple p v)))
    -> k
    -> OrdPSQ k p v
    -> Tuple b (OrdPSQ k p v)
alter f k psq0 =
    let psq1 /\ mbPV = case deleteView k psq0 of
                            Nothing  -> psq0 /\ Nothing
                            Just rec -> rec.queue /\ Just (rec.prio /\ rec.value)
        b /\ mbPV'   = f mbPV
    in case mbPV' of
        Nothing          -> b /\ psq1
        Just (p /\ v)    -> b /\ (insert k p v psq1)

-- | A variant of 'alter' which works on the element with the minimum priority
-- | Unlike 'alter', this variant also allows you to change the key of element.
-- |
-- | Running time: `O(log n)`
alterMin
    :: forall b k p v. Ord k => Ord p
    => (Maybe (ElemRec k p v) -> Tuple b (Maybe (ElemRec k p v)))
    -> OrdPSQ k p v
    -> Tuple b (OrdPSQ k p v)
alterMin f psq0 = case minView psq0 of
    Nothing -> let b /\ mbKPV = f Nothing
               in b /\ insertMay mbKPV psq0
    Just rec -> let b /\ mbKPV = f $ Just { key: rec.key, prio: rec.prio, value: rec.value }
                in b /\ insertMay mbKPV rec.queue
  where
    insertMay Nothing    psq = psq
    insertMay (Just rec) psq = insert rec.key rec.prio rec.value psq

-- | Build a priority search queue from a `Foldable` of { key, value, prio } records.
-- | If the Foldable contains more than one priority and value for the same key, the
-- | last priority and value for the key is retained.
-- |
-- | Running time: O(n*log n)
fromFoldable
    :: forall f k p v. Foldable f => Ord k => Ord p
    => f (ElemRec k p v)
    -> OrdPSQ k p v
fromFoldable = foldr accum empty
  where
    accum rec q = insert rec.key rec.prio rec.value q

-- | Obtain the list of present keys in the queue.
-- |
-- | Running time: `O(n)`
keys :: forall k p v. OrdPSQ k p v -> List k
keys = map _.key <<< toAscList

-- | Convert a queue to a list of { key, value, prio } records. The order of list
-- | is ascending.
-- |
-- | Running time: `O(n)`
toAscList :: forall k p v. OrdPSQ k p v -> List (ElemRec k p v)
toAscList q = seqToList $ go q
  where
    go :: OrdPSQ k p v -> Sequ (ElemRec k p v)
    go t = case tourView t of
        Null                -> emptySequ
        Single (Elem k p v) -> singleSequ { key: k, prio: p, value: v }
        Play tl tr          -> go tl <+> go tr

-- | Insert a new key, priority and value into the queue. If the key is already
-- | present in the queue, then the evicted priority and value can be found the
-- | first element of the returned tuple.
-- |
-- | Running time: `O(log n)`
insertView
    :: forall k p v. Ord k => Ord p
    => k -> p -> v -> OrdPSQ k p v -> Tuple (Maybe (Tuple p v)) (OrdPSQ k p v)
insertView k p x t = case deleteView k t of
    Nothing              -> Nothing /\ insert k p x t
    Just { prio, value } -> Just (prio /\ value) /\ insert k p x t

-- | Delete a key and its priority and value from the queue. If the key was present,
-- | the associated priority and value are returned in addition to the updated queue.
-- |
-- | Running time: `O(log n)`
deleteView
  :: forall k p v
   . Ord k => Ord p => k -> OrdPSQ k p v -> Maybe { prio :: p, value :: v, queue :: OrdPSQ k p v }
deleteView k psq = case psq of
    Void            -> Nothing
    Winner (Elem k' p v) Start _
        | k == k'   -> Just $ { prio: p, value: v, queue: empty }
        | otherwise -> Nothing
    Winner e (RLoser _ e' tl m tr) m'
        | k <= m    ->
            map (\rec -> { prio: rec.prio, value: rec.value, queue: rec.queue `play` (Winner e' tr m')})
                (deleteView k (Winner e tl m))
        | otherwise ->
            map (\rec -> { prio: rec.prio, value: rec.value, queue: (Winner e tl m) `play` rec.queue })
                (deleteView k (Winner e' tr m'))
    Winner e (LLoser _ e' tl m tr) m'
        | k <= m    ->
            map (\rec -> { prio: rec.prio, value: rec.value, queue: rec.queue `play` (Winner e tr m') })
                (deleteView k (Winner e' tl m))
        | otherwise ->
            map (\rec -> { prio: rec.prio, value: rec.value, queue: (Winner e' tl m) `play` rec.queue })
                (deleteView k (Winner e tr m'))

-- | Retrieve an element with the least priority, and the rest of the queue stripped of that binding.
-- |
-- | Running time: `O(log n)`
minView
  :: forall k p v. Ord k => Ord p
  => OrdPSQ k p v -> Maybe { key :: k, prio :: p, value :: v, queue :: OrdPSQ k p v}
minView Void                      = Nothing
minView (Winner (Elem k p v) t m) = Just $ { key: k, prio: p, value: v, queue: secondBest t m }

secondBest :: forall k p v. Ord k => Ord p => LTree k p v -> k -> OrdPSQ k p v
secondBest Start _                 = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'

-- | Map every value on queue, the function passed here will receive all component
-- | of element -- key, prio, value -- and returned value will be used to update the current value.
-- |
-- | Running time: `O(n)`
mapWithKeyPrio :: forall k p v w. (k -> p -> v -> w) -> OrdPSQ k p v -> OrdPSQ k p w
mapWithKeyPrio f = goPSQ
  where
    goPSQ :: OrdPSQ k p v -> OrdPSQ k p w
    goPSQ Void           = Void
    goPSQ (Winner e l k) = Winner (goElem e) (goLTree l) k

    goElem :: Elem k p v -> Elem k p w
    goElem (Elem k p x) = Elem k p (f k p x)

    goLTree :: LTree k p v -> LTree k p w
    goLTree Start              = Start
    goLTree (LLoser s e l k r) = LLoser s (goElem e) (goLTree l) k (goLTree r)
    goLTree (RLoser s e l k r) = RLoser s (goElem e) (goLTree l) k (goLTree r)

-- | Tournament trees
data TourView k p v
    = Null
    | Single (Elem k p v)
    | Play (OrdPSQ k p v) (OrdPSQ k p v)

tourView :: forall k p v. OrdPSQ k p v -> TourView k p v
tourView Void                                = Null
tourView (Winner e Start _)                  = Single e
tourView (Winner e (RLoser _ e' tl m tr) m') = Winner e tl m `Play` Winner e' tr m'
tourView (Winner e (LLoser _ e' tl m tr) m') = Winner e' tl m `Play` Winner e tr m'

-- | Take two pennants and returns a new pennant that is the union of
-- | the two with the precondition that the keys in the ﬁrst tree are
-- | strictly smaller than the keys in the second tree.
play :: forall k p v. Ord k => Ord p => OrdPSQ k p v -> OrdPSQ k p v -> OrdPSQ k p v
play Void t' = t'
play t Void  = t
play (Winner e@(Elem k p v) t m) (Winner e'@(Elem k' p' v') t' m')
    | Tuple p k `beats` Tuple p' k' = Winner e (unsafePartial $ rbalance k' p' v' t m t') m'
    | otherwise                     = Winner e' (unsafePartial $ lbalance k p v t m t') m'

unsafePlay :: forall k p v. Ord k => Ord p => OrdPSQ k p v -> OrdPSQ k p v -> OrdPSQ k p v
unsafePlay Void t' = t'
unsafePlay t Void  = t
unsafePlay (Winner e@(Elem k p v) t m) (Winner e'@(Elem k' p' v') t' m')
    | Tuple p k `beats` Tuple p' k' = Winner e (rloser k' p' v' t m t') m'
    | otherwise                     = Winner e' (lloser k p v t m t') m'

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
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalance k p v l m r
    | size' l + size' r < 2     = lloser        k p v l m r
    | size' r > omega * size' l = lbalanceLeft  k p v l m r
    | size' l > omega * size' r = lbalanceRight k p v l m r
    | otherwise                 = lloser        k p v l m r

rbalance
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalance k p v l m r
    | size' l + size' r < 2     = rloser        k p v l m r
    | size' r > omega * size' l = rbalanceLeft  k p v l m r
    | size' l > omega * size' r = rbalanceRight k p v l m r
    | otherwise                 = rloser        k p v l m r

lbalanceLeft
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = lsingleLeft  k p v l m r
    | otherwise                        = ldoubleLeft  k p v l m r

lbalanceRight
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceRight k p v l m r
    | size' (left l) > size' (right l) = lsingleRight k p v l m r
    | otherwise                        = ldoubleRight k p v l m r

rbalanceLeft
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = rsingleLeft  k p v l m r
    | otherwise                        = rdoubleLeft  k p v l m r

rbalanceRight
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceRight k p v l m r
    | size' (left l) > size' (right l) = rsingleRight k p v l m r
    | otherwise                        = rdoubleRight k p v l m r

lsingleLeft
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3)
    | Tuple p1 k1 `beats` Tuple p2 k2 = lloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
    | otherwise                       = lloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
lsingleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3

rsingleLeft
    :: forall k p v. Partial
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
rsingleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (rloser k1 p1 v1 t1 m1 t2) m2 t3

lsingleRight
    :: forall k p v. Partial
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (lloser k1 p1 v1 t2 m2 t3)
lsingleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)

rsingleRight
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3
    | Tuple p1 k1 `beats` Tuple p2 k2 =
        rloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
    | otherwise                 =
        rloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)

ldoubleLeft
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)

ldoubleRight
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3

rdoubleLeft
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleLeft k1 p1 v1 t1 m1 (LLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft k1 p1 v1 t1 m1 (RLoser _ (Elem k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)

rdoubleRight
    :: forall k p v. Partial => Ord k => Ord p
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleRight k1 p1 v1 (LLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 v1 (RLoser _ (Elem k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3

-- | Hughes's efficient sequence type
newtype Sequ a = Sequ (List a -> List a)

emptySequ :: forall a. Sequ a
emptySequ = Sequ (\as -> as)

singleSequ :: forall a. a -> Sequ a
singleSequ a = Sequ (\as -> a : as)

appendSequ :: forall a. Sequ a -> Sequ a -> Sequ a
appendSequ (Sequ x1) (Sequ x2) = Sequ (\as -> x1 (x2 as))

infixr 5 appendSequ as <+>

seqToList :: forall a. Sequ a -> List a
seqToList (Sequ x) = x Nil
