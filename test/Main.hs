module Main where

import Data.Function (($))
import System.IO (IO)
import Test.Tasty

import HUnit

main :: IO ()
main =
	defaultMain $
	testGroup "TreeMap"
	 [ hunits
	 ]
