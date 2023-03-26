{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements a strict 'TreeMap',
-- which is like a 'Map'
-- but whose key is now a 'NonNull' list of 'Map' keys (a 'Path')
-- enabling the possibility to gather mapped values
-- by 'Path' prefixes (inside a 'Node').
module Data.TreeMap.Strict where

import Control.Applicative (Applicative(..), Alternative((<|>)))
import Control.DeepSeq (NFData(..))
import Control.Monad (Monad(..))
import Data.Bool
import Data.Data (Data)
import Data.Eq (Eq(..))
import Data.Foldable (Foldable, foldMap)
import Data.Function (($), (.), const, flip, id)
import Data.Functor (Functor(..), (<$>))
import Data.Map.Strict (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (Monoid(..))
import Data.NonNull (NonNull, nuncons, toNullable)
import Data.Ord (Ord(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequences (reverse)
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import Prelude (Int, Num(..), seq)
import Text.Show (Show(..))
import qualified Control.Applicative as App
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonNull as NonNull
import qualified Data.Strict.Maybe as Strict

-- @Data.Strict@ orphan instances
deriving instance Typeable Strict.Maybe
instance Applicative Strict.Maybe where
	pure = Strict.Just
	Strict.Just f <*> Strict.Just x = Strict.Just (f x)
	_ <*> _ = Strict.Nothing
instance Alternative Strict.Maybe where
	empty = Strict.Nothing
	x <|> y = if Strict.isJust x then x else y

-- * Type 'TreeMap'
newtype TreeMap k x
 =      TreeMap (Map k (Node k x))
 deriving (Eq, Ord, Show, Typeable)

instance (Ord k, Semigroup v) => Semigroup (TreeMap k v) where
	(<>) = union (<>)
instance (Ord k, Monoid v) => Monoid (TreeMap k v) where
	mempty = empty
	mappend = union mappend
	-- mconcat = List.foldr mappend mempty
instance Ord k => Functor (TreeMap k) where
	fmap f (TreeMap m) = TreeMap $ fmap (fmap f) m
instance Ord k => Foldable (TreeMap k) where
	foldMap f (TreeMap m) = foldMap (foldMap f) m
instance Ord k => Traversable (TreeMap k) where
	traverse f (TreeMap m) = TreeMap <$> traverse (traverse f) m
instance (Ord k, NFData k, NFData x) => NFData (TreeMap k x) where
	rnf (TreeMap m) = rnf m

-- * Type 'Path'
-- | A 'Path' is a non-empty list of 'Map' keys.
type Path k = NonNull [k]

-- | 'Path' constructor.
path :: k -> [k] -> Path k
path = NonNull.ncons

-- | Convenient alias.
(<|) :: k -> [k] -> Path k
(<|) = path

-- * Type 'Node'
data Node k x
 =   Node
 {   node_size        :: !Int -- ^ The number of non-'Strict.Nothing' 'node_value's reachable from this 'Node'.
 ,   node_value       :: !(Strict.Maybe x) -- ^ Some value, or 'Strict.Nothing' if this 'Node' is intermediary.
 ,   node_descendants :: !(TreeMap k x) -- ^ Descendants 'Node's.
 } deriving (Eq, Ord, Show, Typeable)

instance (Ord k, Semigroup v) => Semigroup (Node k v) where
	(<>)
	 Node{node_value=x0, node_descendants=m0}
	 Node{node_value=x1, node_descendants=m1} =
		node (x0 <> x1) (union const m0 m1)
instance (Ord k, Semigroup v) => Monoid (Node k v) where
	mempty = node Strict.Nothing (TreeMap mempty)
	mappend = (<>)
	-- mconcat = List.foldr mappend mempty
instance Ord k => Functor (Node k) where
	fmap f Node{node_value=x, node_descendants=m, node_size} =
		Node
		 { node_value = fmap f x
		 , node_descendants = map f m
		 , node_size
		 }
instance Ord k => Foldable (Node k) where
	foldMap f Node{node_value=Strict.Nothing, node_descendants=TreeMap m} =
		foldMap (foldMap f) m
	foldMap f Node{node_value=Strict.Just x, node_descendants=TreeMap m} =
		f x `mappend` foldMap (foldMap f) m
instance Ord k => Traversable (Node k) where
	traverse f Node{node_value=Strict.Nothing, node_descendants=TreeMap m, node_size} =
		Node node_size <$> pure Strict.Nothing <*> (TreeMap <$> traverse (traverse f) m)
	traverse f Node{node_value=Strict.Just x, node_descendants=TreeMap m, node_size} =
		Node node_size <$> (Strict.Just <$> f x) <*> (TreeMap <$> traverse (traverse f) m)
instance (Ord k, NFData k, NFData x) => NFData (Node k x) where
	rnf (Node s v d) = rnf s `seq` rnf v `seq` rnf d

node :: Strict.Maybe x -> TreeMap k x -> Node k x
node node_value node_descendants =
	Node
	 { node_value
	 , node_size =
		size node_descendants +
		Strict.maybe 0 (const 1) node_value
	 , node_descendants
	 }

nodeEmpty :: Node k x
nodeEmpty = node Strict.Nothing empty

nodeLookup :: Ord k => [k] -> Node k x -> Strict.Maybe (Node k x)
nodeLookup [] n = Strict.Just n
nodeLookup (k:ks) Node{node_descendants=TreeMap m} =
	maybe Strict.Nothing (nodeLookup ks) $
	Map.lookup k m

-- * Construct

-- | Return the empty 'TreeMap'.
empty :: TreeMap k x
empty = TreeMap Map.empty

-- | Return a 'TreeMap' only mapping the given 'Path' to the given value.
singleton :: Ord k => Path k -> x -> TreeMap k x
singleton ks x = insert const ks x empty

-- | Return a 'Node' only containing the given value.
leaf :: x -> Node k x
leaf x = node (Strict.Just x) empty

-- | Return the given 'TreeMap' associating the given 'Path' with the given value,
-- merging values if the given 'TreeMap' already associates the given 'Path'
-- with a non-'Strict.Nothing' 'node_value'.
insert :: Ord k => (x -> x -> x) -> Path k -> x -> TreeMap k x -> TreeMap k x
insert merge p x (TreeMap m) =
	TreeMap $
	case nuncons p of
	 (k, Nothing) ->
		Map.insertWith (\_ Node{..} -> node
			 (Strict.maybe (Strict.Just x) (Strict.Just . merge x) node_value)
			 node_descendants)
		 k (leaf x) m
	 (k, Just p') ->
		Map.insertWith (\_ Node{..} -> node node_value $
			insert merge p' x node_descendants)
		 k (node Strict.Nothing (insert merge p' x empty)) m

-- | Return a 'TreeMap' from a list of 'Path'/value pairs,
-- with a combining function called on the leftest and rightest values
-- when their 'Path's are identical.
fromList :: Ord k => (x -> x -> x) -> [(Path k, x)] -> TreeMap k x
fromList merge = List.foldl' (\acc (p,x) -> insert merge p x acc) empty

-- | Return a 'TreeMap' from a 'Map' mapping 'Path' to value.
fromMap :: Ord k => Map (Path k) x -> TreeMap k x
fromMap = go . Map.toList
	where
	go :: Ord k => [(Path k,x)] -> TreeMap k x
	go m =
		TreeMap $ Map.fromAscListWith
		 (\Node{node_value=vn, node_descendants=mn}
		   Node{node_value=vo, node_descendants=mo} ->
			node (vn <|> vo) $ union const mn mo) $
		(<$> m) $ \(p,x) ->
			let (p0,mps) = nuncons p in
			case mps of
			 Nothing -> (p0,node (Strict.Just x) empty)
			 Just ps -> (p0,node Strict.Nothing $ go [(ps,x)])
-- fromMap = Map.foldlWithKey (\acc p x -> insert const p x acc) empty

-- * Size

-- | Return the 'Map' in the given 'TreeMap'.
nodes :: TreeMap k x -> Map k (Node k x)
nodes (TreeMap m) = m

-- | Return 'True' iif. the given 'TreeMap' is of 'size' @0@.
null :: TreeMap k x -> Bool
null m = size m == 0

-- | Return the number of non-'Strict.Nothing' 'node_value's in the given 'TreeMap'.
--
--   * Complexity: O(r) where r is the size of the root 'Map'.
size :: TreeMap k x -> Int
size = Map.foldr ((+) . node_size) 0 . nodes

-- * Find

-- | Return the value (if any) associated with the given 'Path'.
lookup :: Ord k => Path k -> TreeMap k x -> Strict.Maybe x
lookup p (TreeMap m) =
	maybe Strict.Nothing nod_val $ Map.lookup k m
	where
	(k, mp') = nuncons p
	nod_val =
		case mp' of
		 Nothing -> node_value
		 Just p' -> lookup p' . node_descendants

-- | Return the values (if any) associated with the prefixes of the given 'Path' (included).
lookupAlong :: Ord k => Path k -> TreeMap k x -> [x]
lookupAlong p (TreeMap tm) =
	go (toNullable p) tm
	where
		go :: Ord k => [k] -> Map k (Node k x) -> [x]
		go [] _m = []
		go (k:ks) m =
			case Map.lookup k m of
			 Nothing -> []
			 Just nod ->
				Strict.maybe id (:) (node_value nod) $
				go ks $ nodes (node_descendants nod)

-- | Return the 'Node' (if any) associated with the given 'Path'.
lookupNode :: Ord k => Path k -> TreeMap k x -> Maybe (Node k x)
lookupNode p (TreeMap m) =
	case nuncons p of
	 (k, Nothing) -> Map.lookup k m
	 (k, Just p') -> Map.lookup k m >>= lookupNode p' . node_descendants

-- * Union

-- | Return a 'TreeMap' associating the same 'Path's as both given 'TreeMap's,
-- merging values (in respective order) when a 'Path' leads
-- to a non-'Strict.Nothing' 'node_value' in both given 'TreeMap's.
union :: Ord k => (x -> x -> x) -> TreeMap k x -> TreeMap k x -> TreeMap k x
union merge (TreeMap tm0) (TreeMap tm1) =
	TreeMap $
	Map.unionWith
	 (\Node{node_value=x0, node_descendants=m0}
	   Node{node_value=x1, node_descendants=m1} ->
		node (Strict.maybe x1 (\x0' -> Strict.maybe (Strict.Just x0') (Strict.Just . merge x0') x1) x0)
		 (union merge m0 m1))
	 tm0 tm1

-- | Return the 'union' of the given 'TreeMap's.
--
-- NOTE: use |List.foldl'| to reduce demand on the control-stack.
unions :: Ord k => (x -> x -> x) -> [TreeMap k x] -> TreeMap k x
unions merge = List.foldl' (union merge) empty

-- foldl' :: (a -> b -> a) -> a -> [b] -> a
-- foldl' f = go
-- 	where
-- 		go z []     = z
-- 		go z (x:xs) = z `seq` go (f z x) xs

-- * Map

-- | Return the given 'TreeMap' with each non-'Strict.Nothing' 'node_value'
-- mapped by the given function.
map :: Ord k => (x -> y) -> TreeMap k x -> TreeMap k y
map f =
	TreeMap .
	Map.map
	 (\n@Node{node_value=x, node_descendants=m} ->
		n{ node_value       = fmap f x
		 , node_descendants = map f m
		 }) .
	nodes

-- | Return the given 'TreeMap' with each 'Path' section
-- and each non-'Strict.Nothing' 'node_value'
-- mapped by the given functions.
--
-- WARNING: the function mapping 'Path' sections must be monotonic,
-- like in 'Map.mapKeysMonotonic'.
mapMonotonic :: (Ord k, Ord l) => (k -> l) -> (x -> y) -> TreeMap k x -> TreeMap l y
mapMonotonic fk fx =
	TreeMap .
	Map.mapKeysMonotonic fk .
	Map.map
	 (\n@Node{node_value=x, node_descendants=m} ->
		n{ node_value       = fmap fx x
		 , node_descendants = mapMonotonic fk fx m
		 }) .
	nodes

-- | Return the given 'TreeMap' with each 'node_value'
-- mapped by the given function supplied with
-- the already mapped 'node_descendants' of the current 'Node'.
mapByDepthFirst :: Ord k => (TreeMap k y -> Strict.Maybe x -> y) -> TreeMap k x -> TreeMap k y
mapByDepthFirst f =
	TreeMap .
	Map.map
	 (\Node{node_value, node_descendants} ->
		let m = mapByDepthFirst f node_descendants in
		node (Strict.Just $ f m node_value) m) .
	nodes

-- * Alter

alterl_path :: Ord k => (Strict.Maybe x -> Strict.Maybe x) -> Path k -> TreeMap k x -> TreeMap k x
alterl_path fct =
	go fct . toNullable
	where
		go :: Ord k
		 => (Strict.Maybe x -> Strict.Maybe x) -> [k]
		 -> TreeMap k x -> TreeMap k x
		go _f [] m = m
		go f (k:p) (TreeMap m) =
			TreeMap $
			Map.alter
			 (\c ->
				let (cv, cm) =
					case c of
					 Just Node{node_value=v, node_descendants=d} -> (v, d)
					 Nothing -> (Strict.Nothing, empty) in
				let fx = f cv in
				let gm = go f p cm in
				case (fx, size gm) of
				 (Strict.Nothing, 0) -> Nothing
				 (_, s) -> Just
					Node
					 { node_value = fx
					 , node_descendants = gm
					 , node_size = s + 1
					 }
			 ) k m

-- * Fold

-- | Return the given accumulator folded by the given function
-- applied on non-'Strict.Nothing' 'node_value's
-- from left to right through the given 'TreeMap'.
foldlWithPath :: Ord k => (a -> Path k -> x -> a) -> a -> TreeMap k x -> a
foldlWithPath =
	foldp []
	where
		foldp :: Ord k
		 => [k] -> (a -> Path k -> x -> a)
		 -> a -> TreeMap k x -> a
		foldp p fct a (TreeMap m) =
			Map.foldlWithKey
			 (\acc k Node{..} ->
				let acc' = Strict.maybe acc (fct acc (reverse $ path k p)) node_value in
				foldp (k:p) fct acc' node_descendants) a m

-- | Return the given accumulator folded by the given function
-- applied on non-'Strict.Nothing' 'Node's and 'node_value's
-- from left to right through the given 'TreeMap'.
foldlWithPathAndNode :: Ord k => (a -> Node k x -> Path k -> x -> a) -> a -> TreeMap k x -> a
foldlWithPathAndNode =
	foldp []
	where
		foldp :: Ord k
		 => [k] -> (a -> Node k x -> Path k -> x -> a)
		 -> a -> TreeMap k x -> a
		foldp p fct a (TreeMap m) =
			Map.foldlWithKey
			 (\acc k n@Node{..} ->
				let acc' = Strict.maybe acc (fct acc n (reverse $ path k p)) node_value in
				foldp (k:p) fct acc' node_descendants) a m

-- | Return the given accumulator folded by the given function
-- applied on non-'Strict.Nothing' 'node_value's
-- from right to left through the given 'TreeMap'.
foldrWithPath :: Ord k => (Path k -> x -> a -> a) -> a -> TreeMap k x -> a
foldrWithPath =
	foldp []
	where
		foldp :: Ord k
		 => [k] -> (Path k -> x -> a -> a)
		 -> a -> TreeMap k x -> a
		foldp p fct a (TreeMap m) =
			Map.foldrWithKey
			 (\k Node{..} acc ->
				let acc' = foldp (k:p) fct acc node_descendants in
				Strict.maybe acc' (\x -> fct (reverse $ path k p) x acc') node_value) a m

-- | Return the given accumulator folded by the given function
-- applied on non-'Strict.Nothing' 'Node's and 'node_value's
-- from right to left through the given 'TreeMap'.
foldrWithPathAndNode :: Ord k => (Node k x -> Path k -> x -> a -> a) -> a -> TreeMap k x -> a
foldrWithPathAndNode =
	foldp []
	where
		foldp :: Ord k
		 => [k] -> (Node k x -> Path k -> x -> a -> a)
		 -> a -> TreeMap k x -> a
		foldp p fct a (TreeMap m) =
			Map.foldrWithKey
			 (\k n@Node{..} acc ->
				let acc' = foldp (k:p) fct acc node_descendants in
				Strict.maybe acc' (\x -> fct n (reverse $ path k p) x acc') node_value) a m

-- | Return the given accumulator folded by the given function
-- applied on non-'Strict.Nothing' 'node_value's
-- from left to right along the given 'Path'.
foldlPath :: Ord k => (Path k -> x -> a -> a) -> Path k -> TreeMap k x -> a -> a
foldlPath fct =
	go fct [] . toNullable
	where
		go :: Ord k
		 => (Path k -> x -> a -> a) -> [k] -> [k]
		 -> TreeMap k x -> a -> a
		go _f _ [] _t a = a
		go f p (k:n) (TreeMap t) a =
			case Map.lookup k t of
			 Nothing -> a
			 Just Node{..} ->
				case node_value of
				 Strict.Nothing -> go f (k:p) n node_descendants a
				 Strict.Just x  -> go f (k:p) n node_descendants (f (reverse $ path k p) x a)

-- | Return the given accumulator folded by the given function
-- applied on non-'Strict.Nothing' 'node_value's
-- from right to left along the given 'Path'.
foldrPath :: Ord k => (Path k -> x -> a -> a) -> Path k -> TreeMap k x -> a -> a
foldrPath fct =
	go fct [] . toNullable
	where
		go :: Ord k
		 => (Path k -> x -> a -> a) -> [k] -> [k]
		 -> TreeMap k x -> a -> a
		go _f _ [] _t a = a
		go f p (k:n) (TreeMap t) a =
			case Map.lookup k t of
			 Nothing -> a
			 Just Node{..} ->
				case node_value of
				 Strict.Nothing -> go f (k:p) n node_descendants a
				 Strict.Just x  -> f (reverse $ path k p) x $ go f (k:p) n node_descendants a

-- * Flatten

-- | Return a 'Map' associating each 'Path'
-- leading to a non-'Strict.Nothing' 'node_value' in the given 'TreeMap',
-- with its value mapped by the given function.
flatten :: Ord k => (x -> y) -> TreeMap k x -> Map (Path k) y
flatten = flattenWithPath . const

-- | Like 'flatten' but with also the current 'Path' given to the mapping function.
flattenWithPath :: Ord k => (Path k -> x -> y) -> TreeMap k x -> Map (Path k) y
flattenWithPath =
	flat_map []
	where
		flat_map :: Ord k
		 => [k] -> (Path k -> x -> y)
		 -> TreeMap k x
		 -> Map (Path k) y
		flat_map p f (TreeMap m) =
			Map.unions $
			Map.mapKeysMonotonic (reverse . flip path p) (
			Map.mapMaybeWithKey (\k Node{node_value} ->
				case node_value of
				 Strict.Nothing -> Nothing
				 Strict.Just x  -> Just $ f (reverse $ path k p) x) m
			) :
			Map.foldrWithKey
			 (\k -> (:) . flat_map (k:p) f . node_descendants)
			 [] m

-- * Filter

-- | Return the given 'TreeMap'
--   keeping only its non-'Strict.Nothing' 'node_value's
--   passing the given predicate.
filter :: Ord k => (x -> Bool) -> TreeMap k x -> TreeMap k x
filter f =
	mapMaybeWithPath
	 (\_p x -> if f x then Strict.Just x else Strict.Nothing)

-- | Like 'filter' but with also the current 'Path' given to the predicate.
filterWithPath :: Ord k => (Path k -> x -> Bool) -> TreeMap k x -> TreeMap k x
filterWithPath f =
	mapMaybeWithPath
	 (\p x -> if f p x then Strict.Just x else Strict.Nothing)

-- | Like 'filterWithPath' but with also the current 'Node' given to the predicate.
filterWithPathAndNode :: Ord k => (Node k x -> Path k -> x -> Bool) -> TreeMap k x -> TreeMap k x
filterWithPathAndNode f =
	mapMaybeWithPathAndNode
	 (\n p x -> if f n p x then Strict.Just x else Strict.Nothing)

-- | Return the given 'TreeMap'
--   mapping its non-'Strict.Nothing' 'node_value's
--   and keeping only the non-'Strict.Nothing' results.
mapMaybe :: Ord k => (x -> Strict.Maybe y) -> TreeMap k x -> TreeMap k y
mapMaybe = mapMaybeWithPath . const

-- | Like 'mapMaybe' but with also the current 'Path' given to the predicate.
mapMaybeWithPath :: Ord k => (Path k -> x -> Strict.Maybe y) -> TreeMap k x -> TreeMap k y
mapMaybeWithPath = mapMaybeWithPathAndNode . const

-- | Like 'mapMaybeWithPath' but with also the current 'Node' given to the predicate.
mapMaybeWithPathAndNode :: Ord k => (Node k x -> Path k -> x -> Strict.Maybe y) -> TreeMap k x -> TreeMap k y
mapMaybeWithPathAndNode =
	go []
	where
		go :: Ord k
		 => [k] -> (Node k x -> Path k -> x -> Strict.Maybe y)
		 -> TreeMap k x
		 -> TreeMap k y
		go p test (TreeMap m) =
			TreeMap $
			Map.mapMaybeWithKey
			 (\k nod@Node{node_value=v, node_descendants=ns} ->
				let node_descendants = go (k:p) test ns in
				let node_size = size node_descendants in
				case v of
				 Strict.Just x ->
					let node_value = test nod (reverse $ path k p) x in
					case node_value of
					 Strict.Nothing | null node_descendants -> Nothing
					 Strict.Nothing -> Just Node{node_value, node_descendants, node_size=1 + node_size}
					 Strict.Just _  -> Just Node{node_value, node_descendants, node_size}
				 _ ->
					if null node_descendants
					then Nothing
					else Just Node{node_value=Strict.Nothing, node_descendants, node_size}
			 ) m

-- * Intersection

(\\) :: Ord k => TreeMap k x -> TreeMap k y -> TreeMap k x
(\\) = intersection const

intersection ::
 Ord k =>
 (Strict.Maybe x -> Strict.Maybe y -> Strict.Maybe z) ->
 TreeMap k x -> TreeMap k y -> TreeMap k z
intersection merge (TreeMap x) (TreeMap y) =
	TreeMap $
	Map.intersectionWith
	 (\xn yn ->
		node (node_value xn `merge` node_value yn) $
		intersection merge
		 (node_descendants xn)
		 (node_descendants yn))
	 x y
