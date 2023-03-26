module HUnit.Strict where

import Data.Function (($), id, const)
import Data.Int (Int)
import Data.Monoid ((<>))
import Prelude (Integer, undefined)
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict

import Test.Tasty
import Test.Tasty.HUnit

import Data.TreeMap.Strict (TreeMap(..), (<|))
import qualified Data.TreeMap.Strict as TreeMap

hunits :: TestTree
hunits = testGroup "Strict"
 [ testGroup "insert"
	 [ testCase "[] 0" $
			TreeMap.insert const ((0::Int)<|[]) () TreeMap.empty
		 @?= (TreeMap $ Map.fromList [ (0::Int, TreeMap.leaf ()) ])
	 , testCase "[] 0/1" $
			TreeMap.insert const ((0::Int)<|[1]) () TreeMap.empty
		 @?=
			(TreeMap $
			Map.fromList
			 [ (0::Int, TreeMap.Node
				 { TreeMap.node_value = Strict.Nothing
				 , TreeMap.node_size = 1
				 , TreeMap.node_descendants =
					TreeMap.singleton ((1::Int)<|[]) ()
				 })
			 ])
	 ]
 , testGroup "mapByDepthFirst"
	 [ testCase "[0, 0/1, 0/1/2, 1, 1/2/3]" $
			TreeMap.mapByDepthFirst
			 (\descendants value ->
				Map.foldl'
				 (\acc v -> (<>) acc $
					Strict.fromMaybe undefined $
					TreeMap.node_value v
				 )
				 (Strict.fromMaybe [] value)
				 (TreeMap.nodes descendants)
			 )
			(TreeMap.fromList const
			 [ ((0::Integer)<|[], [0::Integer])
			 , (0<|[1], [0,1])
			 , (0<|[1,2], [0,1,2])
			 , (1<|[], [1])
			 , (1<|[2,3], [1,2,3])
			 ])
		 @?=
			TreeMap.fromList const
			 [ ((0::Integer)<|[], [0,0,1,0,1,2])
			 , (0<|[1], [0,1,0,1,2])
			 , (0<|[1,2], [0,1,2])
			 , (1<|[], [1,1,2,3])
			 , (1<|[2], [1,2,3])
			 , (1<|[2,3], [1,2,3])
			 ]
	 , testCase "[0/0]" $
			TreeMap.mapByDepthFirst
			 (\descendants value ->
				Map.foldl'
				 (\acc v -> (<>) acc $
					Strict.fromMaybe undefined $
					TreeMap.node_value v
				 )
				 (Strict.fromMaybe [] value)
				 (TreeMap.nodes descendants)
			 )
			(TreeMap.fromList const
			 [ ((0::Integer)<|[0], [0::Integer,0])
			 ])
		 @?=
			TreeMap.fromList const
			 [ ((0::Integer)<|[], [0,0])
			 , (0<|[0], [0,0])
			 ]
	 ]
 , testGroup "flatten"
	 [ testCase "[0, 0/1, 0/1/2]" $
			TreeMap.flatten id
			(TreeMap.fromList const
			 [ ((0::Integer)<|[], ())
			 , (0<|[1], ())
			 , (0<|[1,2], ())
			 ])
		 @?=
			Map.fromList
			 [ ((0::Integer)<|[], ())
			 , (0<|[1], ())
			 , (0<|[1,2], ())
			 ]
	 , testCase "[1, 1/2, 1/22, 1/2/3, 1/2/33, 11, 11/2, 11/2/3, 11/2/33]" $
			TreeMap.flatten id
			(TreeMap.fromList const
			 [ ((1::Integer)<|[], ())
			 , (1<|[2], ())
			 , (1<|[22], ())
			 , (1<|[2,3], ())
			 , (1<|[2,33], ())
			 , (11<|[], ())
			 , (11<|[2], ())
			 , (11<|[2,3], ())
			 , (11<|[2,33], ())
			 ])
		 @?=
			Map.fromList
			 [ ((1::Integer)<|[], ())
			 , (1<|[2], ())
			 , (1<|[22], ())
			 , (1<|[2,3], ())
			 , (1<|[2,33], ())
			 , (11<|[], ())
			 , (11<|[2], ())
			 , (11<|[2,3], ())
			 , (11<|[2,33], ())
			 ]
	 ]
 , testGroup "lookupAlong"
	 [ testCase "0/1/2/3 [0, 0/1, 0/1/2, 0/1/2/3]" $
			TreeMap.lookupAlong
			 (0<|[1,2,3])
			(TreeMap.fromList const
			 [ ((0::Integer)<|[], [0])
			 , (0<|[1], [0,1])
			 , (0<|[1,2], [0,1,2])
			 , (0<|[1,2,3], [0,1,2,3])
			 ])
		 @?=
			[ [0::Integer]
			, [0,1]
			, [0,1,2]
			, [0,1,2,3]
			]
	 , testCase "0/1/2/3 [0, 0/1]" $
			TreeMap.lookupAlong
			 (0<|[1,2,3])
			(TreeMap.fromList const
			 [ ((0::Integer)<|[], [0])
			 , (0<|[1], [0,1])
			 ])
		 @?=
			[ [0::Integer]
			, [0,1]
			]
	 ]
 ]
