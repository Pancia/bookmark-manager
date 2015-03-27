{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} TestBookmarkManager

main :: IO ()
main = htfMainWithArgs ["--colors=true"] htf_importedTests
