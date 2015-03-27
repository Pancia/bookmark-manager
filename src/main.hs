{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, writeFile, showList)

import Control.Conditional hiding (unless, when)
import Control.Monad
import Control.Monad.State
import Data.Maybe
import System.Console.GetOpt (getOpt, ArgOrder(RequireOrder))
import System.Environment

import Utils
import BookmarkManager

main :: (Functor m, MonadIO m, MockIO m) => m ()
main = do args <- liftIO getArgs
          let (actions, _nonOpts, _errs) = getOpt RequireOrder options args
              opts = foldl (flip ($)) defaultOptions actions
              argListTags = optListTags opts
              searchTags = optSearch opts
              argDbFile = optDbFile opts
              argDelete = optDelete opts
              tagsToOpen = optOpenTags opts
              shouldPrompt = optPrompt opts
              shouldStartRepl = optInteractive opts
              (shouldImport, argImportFile) = optImportFile opts
              (shouldExport, argExFile) = optExFile opts
              (argUpdOldTags, argUpdNewTags) = optUpdate opts
              argGetTitles = optGetTitles opts
          bms <- liftM read (readFile argDbFile)
          cond [(argGetTitles
                ,do bms' <- forM bms getBmTitle
                    length bms' `seq` writeFile argDbFile (showList bms'))

               ,(isJust shouldImport
                ,do let importBMs = case fromJust shouldImport of
                                      HTML -> importBMsFromHtml
                                      CSV  -> importBMsFromCSV
                    importBMs bms argDbFile argImportFile)

               ,(shouldExport
                ,exportBMs bms argExFile)

               ,(shouldStartRepl
                ,repl opts bms)

               ,(isJust argListTags
                ,printStats (fromJust argListTags) bms)

               ,(not $ null searchTags
                ,do let searchResults = findByTags bms searchTags
                    mapM_ printBM searchResults)

               ,(not $ null tagsToOpen
                ,mapM_ (openURL shouldPrompt . bmUrl)
                       (findByTags bms tagsToOpen))

               ,(not $ null argUpdOldTags
                ,void $ updateBMs opts bms argUpdOldTags argUpdNewTags)

               ,(not $ null argDelete
                ,void $ deleteBMs opts argDelete bms)

               ,(otherwise,
                repl opts bms)]
