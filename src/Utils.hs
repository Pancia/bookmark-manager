{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils where

import Prelude hiding (putStrLn)
import qualified Prelude

import Control.Concurrent (threadDelay)
import Control.Monad.Identity
import Control.Monad.State
import Data.Char (toLower)
import Data.List.Split
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.IO (hFlush, stdout)
import qualified Data.Map as M
import qualified System.Process as P

import System.IO.Unsafe
import Debug.Trace

--TODO: Move to separate file (Flags.hs or Opts.hs)
data ImportType = CSV | HTML
                deriving (Show)
data Options = Options { optSearch :: [String]
                       , optDbFile :: String
                       , optExFile :: (Bool, String)
                       , optOpenTags :: [String]
                       , optListTags :: Maybe Bool
                       , optDelete :: [String]
                       , optUpdate :: ([String],[String])
                       , optInteractive :: Bool
                       , optPrompt :: Bool
                       , optImportFile :: (Maybe ImportType,String)
                       , optGetTitles :: Bool
                       } deriving (Show)
defaultOptions :: Options
defaultOptions = Options { optSearch = []
                         , optImportFile = (Nothing,"")
                         , optDbFile = "bms.db"
                         , optExFile = (False,"bms.out.json")
                         , optOpenTags = []
                         , optListTags = Nothing
                         , optDelete = []
                         , optUpdate = ([],[])
                         , optInteractive = False
                         , optPrompt = False
                         , optGetTitles = False
                         }
options :: [OptDescr (Options -> Options)]
options = [Option "s" ["search"] (ReqArg readSearch "TAGS")"search by TAGS"
          ,Option "f" ["db-file"] (ReqArg readDbFile "FILE") "use FILE as bm db"
          ,Option "i" ["import"] (OptArg readImport "FILE") "import from FILE to db"
          ,Option "e" ["export"] (OptArg readExFile "FILE") "export as html to FILE"
          ,Option "o" ["open"] (ReqArg readOpenTags "TAGS") "open bms matching TAGS"
          ,Option "t" ["stats"] (OptArg readListTags "OPT") "list all tags"
          ,Option "u" ["update"] (ReqArg readUpdate "TAGS#NTAGS") "replace TAGS with NTAGS"
          ,Option "d" ["delete"] (ReqArg readDelete "TAGS") "delete matching TAGS"
          ,Option "p" ["prompt"] (NoArg readPrompt) "prompt before del/upd/..."
          ,Option "r" ["repl"] (NoArg readInteractive) "enter repl mode"
          ,Option "" ["get-titles"] (NoArg readGetTitles) "get titles for bms with autogen'ed titles"
          ]
    where
        readGetTitles opts = opts {optGetTitles = True}
        readImport arg opts@(Options {optImportFile=(_,dflt)}) =
            opts {optImportFile = (Just argType,arg')}
            where
                arg' = fromMaybe dflt arg
                argType = case map toLower . last $ splitOn "." arg' of
                              "csv" -> CSV
                              "html" -> HTML
                              x -> error $ "Invalid ImportType: " ++ x
        readInteractive opts = opts {optInteractive = True}
        readPrompt opts = opts {optPrompt = True}
        readSearch arg opts = opts {optSearch = splitOn "," arg}
        readDbFile arg opts = opts {optDbFile = arg}
        readExFile arg opts@(Options {optExFile=(_,dflt)}) =
            opts {optExFile = (True,fromMaybe dflt arg)}
        readOpenTags arg opts = opts {optOpenTags = splitOn "," arg}
        readListTags arg opts = opts {optListTags = Just $ maybe False ((== "t") . map toLower) arg}
        readDelete arg opts = opts {optDelete = splitOn "," arg}
        readUpdate arg opts = opts {optUpdate = let [x,y] = splitOn "#" arg
                                                    oldTags = splitOn "," x
                                                    newTags = splitOn "," y
                                                in (oldTags,newTags)}

infixl 0 ?>
(?>) :: (Show a) => a -> String -> a
(?>) a str = trace (str ++ ": " ++ show a) a

-- TODO: Move to separate file (MockIO.hs?)
-- NOTE: FOR TESTING ONLY
instance MonadIO Identity where
        liftIO io = return $ unsafePerformIO io

class Monad m => MockIO m where
        getLine :: m String
        putStr :: String -> m ()
        putStrLn :: String -> m ()
        print :: (Show a) => a -> m ()
        readFile :: String -> m String
        writeFile :: FilePath -> String -> m ()
        appendFile :: FilePath -> String -> m ()
        open :: String -> Bool -> m ()

instance MockIO IO where
        getLine = Prelude.getLine
        putStr s = do Prelude.putStr s
                      hFlush stdout
        putStrLn = Prelude.putStrLn
        print = Prelude.print
        readFile = Prelude.readFile
        writeFile = Prelude.writeFile
        appendFile = Prelude.appendFile
        open url inBg = do
            let inBgFlag = if inBg then "-g" else ""
            x <- P.spawnCommand ("open " ++ inBgFlag ++ url)
            threadDelay (250*1000)
            void $ P.waitForProcess x

data Exe = Open String Bool
         deriving (Show, Eq)
data MockData = MockData {
            mockInput  :: [String],
            mockOutput :: [String],
            mockFiles  :: M.Map FilePath String,
            mockExeLog :: [Exe]
            } deriving (Show, Eq)

instance MockIO (State MockData) where
        readFile reqFile = do
            mockData <- get
            let maybeFile = M.lookup reqFile (mockFiles mockData)
            maybe (fail $ reqFile ++ " not found")
                  return maybeFile
        writeFile filePath contents = do
            mockData <- get
            let mockFiles' = mockFiles mockData
                mockFiles'' = M.alter (\_ -> Just contents) filePath mockFiles'
            put $ mockData {mockFiles = mockFiles''}
        appendFile filePath contents = do
            mockData <- get
            let mockFiles' = mockFiles mockData
                mockFiles'' = M.alter (\x -> Just (fromMaybe "" x ++ contents)) filePath mockFiles'
            put $ mockData {mockFiles = mockFiles''}
        getLine = do
            mockData <- get
            let (line:extra) = mockInput mockData
            put $ mockData {mockInput = extra}
            return line
        putStr s = do
            mockData <- get
            put $ mockData {mockOutput = s : mockOutput mockData}
        putStrLn s = do
            mockData <- get
            put $ mockData {mockOutput = (s ++ "\n") : mockOutput mockData}
        print a = putStrLn (show a)
        open url inBg = do
            mockData <- get
            put $ mockData {mockExeLog = Open url inBg : mockExeLog mockData}
