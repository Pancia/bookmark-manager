{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestBookmarkManager where

import Control.Monad.State
import qualified Data.Map as M
import Data.Set (fromList)

import qualified BookmarkManager as BM
import Utils
import Test.Framework as TF

--(updateBMs,deleteBMs,exportBMs,importBMsFromCSV,importBMsFromHtml,repl,printBM
--,findByTags,getBmTitle,printStats,showBMs)
aURL :: String
aURL = "http://www.google.com"

test_mkBM :: IO ()
test_mkBM = do
       bm <- BM.mkBM ["mkBM"] aURL
       let actual = bm
           expected = BM.BM (fromList ["mkBM"]) aURL "Google"
       assertEqual expected actual

test_addBM :: IO ()
test_addBM = do
      bm <- BM.mkBM ["addBM"] aURL
      let bms = []
          dbFile = "dbFile.db"
          expected = MockData [] [] (M.fromList [(dbFile,"["++show bm++"]")]) []
          actual = execState (BM.addBM dbFile bms bm)
                             (MockData [] [] M.empty [])
      assertEqual expected actual

test_updateBM :: IO ()
test_updateBM = do
        bm <- BM.mkBM ["updBMs"] aURL
        let opts = defaultOptions
            dbFile = optDbFile opts
            bms = [bm]
            oldTags = ["updBMs"]
            newTags = ["updateBMs"]
            expected = MockData [] [] (M.fromList [(dbFile, "[" ++ show (BM.setBmTags newTags bm) ++ "]")]) []
            actual = execState (BM.updateBMs opts bms oldTags newTags)
                               (MockData [] [] M.empty [])
        assertEqual expected actual

test_readYN :: IO ()
test_readYN = do
       let expected = (True, MockData [] ["prompt\n[Y/n]$\n"] M.empty [])
           actual = runState (BM.readYN "prompt")
                             (MockData ["yes"] [] M.empty [])
       assertEqual expected actual

test_openBM :: IO ()
test_openBM = do
       let url = aURL
       bm <- BM.mkBM ["openBM"] url
       let expected = MockData [] [] M.empty [Open url False]
           actual = execState (BM.openBM False bm)
                              (MockData [] [] M.empty [])
       assertEqual expected actual
