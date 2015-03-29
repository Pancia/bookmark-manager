{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} TestBookmarkManager

main :: IO ()
main = do let testArgs = ["--colors=true"
                         ,"--max-cur-ms=5000"]
          htfMainWithArgs testArgs htf_importedTests
