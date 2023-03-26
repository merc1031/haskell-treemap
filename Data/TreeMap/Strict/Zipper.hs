{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Data.TreeMap.Strict.Zipper where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (Monad(..), (>=>))
import Data.Bool (Bool)
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe(..), maybe)
import Data.NonNull (nuncons)
import Data.Ord (Ord(..))
import Data.Tuple (fst)
import Data.Typeable (Typeable)
import Text.Show (Show(..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import Data.TreeMap.Strict (TreeMap(..), Node(..), Path)
import qualified Data.TreeMap.Strict as TreeMap

-- * Type 'Zipper'
data Zipper k a
 =   Zipper
 {   zipper_path :: [Cursor k a]
 ,   zipper_curr :: TreeMap k a
 } deriving (Data, Eq, Show, Typeable)

zipper :: TreeMap k a -> Zipper k a
zipper = Zipper []

root :: Ord k => Zipper k a -> TreeMap k a
root = zipper_curr . List.last . axis_ancestor_or_self

zipath :: Zipper k a -> [k]
zipath z =
	fst . cursor_self <$>
	List.reverse (zipper_path z)

current :: Zipper k a -> TreeMap k a
current = zipper_curr

-- * Type 'Cursor'
data Cursor k a
 =   Cursor
 {   cursor_precedings :: TreeMap k a
 ,   cursor_self       :: (k, Node k a)
 ,   cursor_followings :: TreeMap k a
 } deriving (Data, Eq, Show, Typeable)

-- * Axis
type Axis k a = Zipper k a -> [Zipper k a]
type AxisAlt f k a = Zipper k a -> f (Zipper k a)

-- | Collect all 'Zipper's along a given axis,
--   including the first 'Zipper'.
axis_collect :: (z -> Maybe z) -> z -> [z]
axis_collect f z = z : maybe [] (axis_collect f) (f z)

-- | Collect all 'Zipper's along a given axis,
--   excluding the first 'Zipper'.
axis_collect_without_self :: (z -> Maybe z) -> z -> [z]
axis_collect_without_self f z = maybe [] (axis_collect f) (f z)

-- ** Axis @self@
axis_self :: Zipper k a -> Node k a
axis_self z =
	case z of
	 Zipper{ zipper_path=
	         Cursor{cursor_self=(_, nod)}
	         : _ } -> nod
	 _ -> TreeMap.nodeEmpty

-- ** Axis @child@
axis_child :: Ord k => Axis k a
axis_child z =
	axis_child_first z >>=
	axis_collect axis_following_sibling_nearest

axis_child_lookup
 :: (Ord k, Alternative f)
 => k -> AxisAlt f k a
axis_child_lookup k (Zipper path (TreeMap m)) =
	case Map.splitLookup k m of
	 (_, Nothing, _) -> empty
	 (ps, Just s, fs) ->
		pure Zipper
		 { zipper_path = Cursor (TreeMap ps) (k, s) (TreeMap fs) : path
		 , zipper_curr = TreeMap.node_descendants s
		 }

axis_child_lookups :: (Ord k, Alternative f, Monad f) => Path k -> AxisAlt f k a
axis_child_lookups p =
	case nuncons p of
	 (k, Nothing) -> axis_child_lookup k
	 (k, Just p') -> axis_child_lookup k >=> axis_child_lookups p'

axis_child_first :: Alternative f => AxisAlt f k a
axis_child_first (Zipper path (TreeMap m)) =
	case Map.minViewWithKey m of
	 Nothing -> empty
	 Just ((k', s'), fs') ->
		pure Zipper
		 { zipper_path = Cursor TreeMap.empty (k', s') (TreeMap fs') : path
		 , zipper_curr = TreeMap.node_descendants s'
		 }

axis_child_last :: Alternative f => AxisAlt f k a
axis_child_last (Zipper path (TreeMap m)) =
	case Map.maxViewWithKey m of
	 Nothing -> empty
	 Just ((k', s'), ps') ->
		pure Zipper
		 { zipper_path = Cursor (TreeMap ps') (k', s') TreeMap.empty : path
		 , zipper_curr = TreeMap.node_descendants s'
		 }

-- ** Axis @ancestor@
axis_ancestor :: Ord k => Axis k a
axis_ancestor = axis_collect_without_self axis_parent

axis_ancestor_or_self :: Ord k => Axis k a
axis_ancestor_or_self = axis_collect axis_parent

-- ** Axis @descendant@
axis_descendant_or_self :: Ord k => Axis k a
axis_descendant_or_self =
	collect_child []
	where
		collect_child acc z =
			z : maybe acc
			 (collect_foll acc)
			 (axis_child_first z)
		collect_foll  acc z =
			collect_child
			 (maybe acc
				 (collect_foll acc)
				 (axis_following_sibling_nearest z)
			 ) z

axis_descendant_or_self_reverse :: Ord k => Axis k a
axis_descendant_or_self_reverse z =
	z : List.concatMap
	 axis_descendant_or_self_reverse
	 (List.reverse $ axis_child z)

axis_descendant :: Ord k => Axis k a
axis_descendant = List.tail . axis_descendant_or_self

-- ** Axis @preceding@
axis_preceding_sibling_nearest :: (Ord k, Alternative f) => AxisAlt f k a
axis_preceding_sibling_nearest (Zipper path _curr) =
	case path of
	 [] -> empty
	 Cursor (TreeMap ps) (k, s) (TreeMap fs):steps ->
		case Map.maxViewWithKey ps of
		 Nothing -> empty
		 Just ((k', s'), ps') ->
			pure Zipper
			 { zipper_path = Cursor (TreeMap ps')
			                             (k', s')
			                             (TreeMap $ Map.insert k s fs)
			                 : steps
			 , zipper_curr = TreeMap.node_descendants s'
			 }

axis_preceding_sibling :: Ord k => Axis k a
axis_preceding_sibling = axis_collect_without_self axis_preceding_sibling_nearest

axis_preceding :: Ord k => Axis k a
axis_preceding =
	axis_ancestor_or_self >=>
	axis_preceding_sibling >=>
	axis_descendant_or_self_reverse

-- ** Axis @following@
axis_following_sibling_nearest :: (Ord k, Alternative f) => AxisAlt f k a
axis_following_sibling_nearest (Zipper path _curr) =
	case path of
	 [] -> empty
	 Cursor (TreeMap ps) (k, s) (TreeMap fs):steps ->
		case Map.minViewWithKey fs of
		 Nothing -> empty
		 Just ((k', s'), fs') ->
			pure Zipper
			 { zipper_path = Cursor (TreeMap $ Map.insert k s ps)
			                             (k', s')
			                             (TreeMap fs')
			                 : steps
			 , zipper_curr = TreeMap.node_descendants s'
			 }

axis_following_sibling :: Ord k => Axis k a
axis_following_sibling = axis_collect_without_self axis_following_sibling_nearest

axis_following :: Ord k => Axis k a
axis_following =
	axis_ancestor_or_self >=>
	axis_following_sibling >=>
	axis_descendant_or_self

-- ** Axis @parent@
axis_parent :: (Ord k, Alternative f) => AxisAlt f k a
axis_parent (Zipper path curr) =
	case path of
	 [] -> empty
	 Cursor (TreeMap ps) (k, s) (TreeMap fs):steps ->
		let nod = TreeMap.node (TreeMap.node_value s) curr in
		pure Zipper
		 { zipper_path = steps
		 , zipper_curr = TreeMap $ Map.union ps $ Map.insert k nod fs
		 }

-- ** Filter
axis_filter :: Axis k a -> (Zipper k a -> Bool) -> Axis k a
axis_filter axis p z = List.filter p (axis z)
infixl 5 `axis_filter`

axis_at :: Alternative f => Axis k a -> Int -> AxisAlt f k a
axis_at axis n z = case List.drop n (axis z) of {[] -> empty; a:_ -> pure a}
infixl 5 `axis_at`

zipper_null :: Axis k a -> Zipper k a -> Bool
zipper_null axis = List.null . axis
