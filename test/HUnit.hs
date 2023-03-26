module HUnit where

import Test.Tasty
import qualified HUnit.Strict as Strict

hunits :: TestTree
hunits =
	testGroup "HUnit"
	 [ Strict.hunits
	 ]
