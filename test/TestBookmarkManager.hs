{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestBookmarkManager where

import Control.Monad.State
import qualified Data.Map as M

import BookmarkManager
import Utils

import Test.Framework as TF

test_readYN :: IO ()
test_readYN = do
        let expected = MockData [] ["foo\n[Y/n]$\n"] M.empty []
            actual = execState (readYN "foo")
                               (MockData ["yes"] [] M.empty [])
        assertEqual expected actual

test_openURL :: IO ()
test_openURL = do
        let url = "http://www.google.com"
            expected = MockData [] [] M.empty [Open url False]
            actual = execState (openURL False url)
                               (MockData [] [] M.empty [])
        assertEqual expected actual
