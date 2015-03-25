module Main where

import Prelude hiding (showList)

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Conditional hiding (unless, when)
import Control.Exception
import Control.Monad
import Data.Char (toLower)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (Set, difference, fromList, toList)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Posix.Signals
import System.Process
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Printf

import System.IO.Unsafe

data ImportType = CSV | HTML deriving (Show)
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
                         }

options :: [OptDescr (Options -> Options)]
options = [Option "s" ["search"] (ReqArg readSearch "TAGS")"search by TAGS"
          ,Option "f" ["db-file"] (ReqArg readDbFile "FILE") "use FILE as bm db"
          ,Option "i" ["import"] (OptArg readImport "FILE") "import from FILE to db"
          ,Option "e" ["export"] (OptArg readExFile "FILE") "export as html to FILE"
          ,Option "o" ["open"] (ReqArg readOpenTags "TAGS") "open matching TAGS"
          ,Option "t" ["list-tags"] (OptArg readListTags "OPT") "list all tags"
          ,Option "u" ["update"] (ReqArg readUpdate "TAGS#NTAGS") "replace TAGS with NTAGS"
          ,Option "d" ["delete"] (ReqArg readDelete "TAGS") "delete matching TAGS"
          ,Option "p" ["prompt"] (NoArg readPrompt) "prompt before del/upd/..."
          ,Option "r" ["repl"] (NoArg readInteractive) "enter repl mode"
          ]
    where
        readImport arg opts@(Options {optImportFile=(_,dflt)}) =
            opts {optImportFile = (Just argType,arg')}
            where
                arg' = fromMaybe dflt arg
                argType = case map toLower $ last (splitOn "." arg') of
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

type TAG = String
type URL = String
type TITLE = String
data BM = BM { bmTags :: Set TAG
             , bmUrl :: URL
             , bmTitle :: TITLE
             } deriving (Show, Read, Eq, Ord)

ignoreSignal :: Signal -> IO a -> IO a
ignoreSignal sig = bracket (install Ignore) install . const
  where install handler = installHandler sig handler Nothing

main :: IO ()
main = do args <- getArgs
          let (actions, _nonOpts, _errs) = getOpt RequireOrder options args
              opts = foldl (flip ($)) defaultOptions actions
              argListTags = optListTags opts
              argSearch = optSearch opts
              argDbFile = optDbFile opts
              argDelete = optDelete opts
              argOpenTags = optOpenTags opts
              argPrompt = optPrompt opts
              argInteractive = optInteractive opts
              (argImport, argImportFile) = optImportFile opts
              (argEx, argExFile) = optExFile opts
              (argUpdOldTags, argUpdNewTags) = optUpdate opts
          bms <- liftM read (readFile argDbFile)
          when (isJust argImport) $ (case fromJust argImport of
                                         HTML -> importBMsFromHtml
                                         CSV  -> importBMsFromCSV)
                                    bms argImportFile argDbFile
                                 >> exitSuccess
          when argEx $ exportBMs bms argExFile >> exitSuccess
          when argInteractive $ repl opts bms >> exitSuccess
          when (isJust argListTags) $ printStats (fromJust argListTags) bms
                                   >> exitSuccess
          unless (null argSearch) $ let searchResults = findByTags bms argSearch
                                    in mapM_ printBM searchResults >> exitSuccess
          unless (null argOpenTags) $ mapM_ (open argPrompt . bmUrl)
                                            (findByTags bms argOpenTags)
                                   >> exitSuccess
          unless (null argUpdOldTags) $ updateBMs opts bms argUpdOldTags argUpdNewTags
                                     >> exitSuccess
          unless (null argDelete) $ removeBMs opts argDelete bms
                                 >> exitSuccess

repl :: Options -> [BM] -> IO ()
repl opts bms = ignoreSignal sigINT $ do
    putStr "[Tags]?"
    tags <- liftM (splitOn ",") getLine
    cond [(tags `elem` [["exit"],["stop"],["abort"],["quit"]],
          do writeFile ".done" "done!\n"
             print "Shutting Down")
         ,(otherwise,
          do let bms' = findByTags bms tags
             putStr . showList $ bms'
             putStrLn $ "Num of found items: " ++ show (length bms') ++ "\n"
             repl opts bms)]

printStats :: Bool -> [BM] -> IO ()
printStats shouldConcat bms
    | shouldConcat = do let freqs = frequencies tags
                        putStrLn . showList $ freqs
                        putStrLn $ "Number of Tag Combinations: " ++ show (length freqs)
                        putStrLn $ "Total: " ++ numBMs
    | otherwise    = do let freqs = frequencies . concat $ tags
                        putStrLn . showList $ freqs
                        putStrLn $ "Number of Tags: " ++ show (length freqs)
                        putStrLn $ "Total: " ++ numBMs
    where tags = map (toList . bmTags) bms
          numBMs = show (length bms)

(=?<) :: BM -> [String] -> Bool
(=?<) (BM{bmTags=tags}) searchTags
    | null tags' = False
    | otherwise = tags' `allElemOf` searchTags
    where
        tags' = toList tags
        allElemOf :: (Eq a) => [a] -> [a] -> Bool
        allElemOf xs = all (`elem` xs)

findByTags :: [BM] -> [String] -> [BM]
findByTags bms tags = filter (=?< tags) bms

updateBMs :: Options -> [BM] -> [String] -> [String] -> IO ()
updateBMs opts bms oldTags newTags = ignoreSignal keyboardSignal $ do
        let outFile = optDbFile opts
            shouldPrompt = optPrompt opts
        newBms <- forM bms (promptThenReplace shouldPrompt)
        length bms `seq` writeFile outFile (showList newBms)
    where
        promptThenReplace :: Bool -> BM -> IO BM
        promptThenReplace shouldPrompt bm =
            if (bm =?< oldTags) && (not shouldPrompt || unsafePerformIO (printBM bm >> readYN))
                then return $ replace bm oldTags newTags
                else return bm
        replace :: BM -> [String] -> [String] -> BM
        replace bm@(BM{bmTags=tags}) old new =
            let tags' = filter (not . (`elem` old)) (toList tags)
            in bm {bmTags=fromList $ tags' ++ new}

removeBMs :: Options -> [String] -> [BM] -> IO ()
removeBMs opts tags bms = ignoreSignal sigINT $ do
        let outFile = optDbFile opts
            _prompt = optPrompt opts
            bms' = difference (fromList bms) (fromList $ findByTags bms tags)
        length bms `seq` writeFile outFile . showList $ toList bms'

showList :: (Show a) => [a] -> String
showList list = "[" ++ intercalate "\n," (map show list) ++ "]\n"

printBM :: BM -> IO ()
printBM = print . (bmTags &&& bmUrl)

frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies = map (head &&& length) . group . sort

open :: Bool -> URL -> IO (Maybe String)
open shouldPrompt url = ignoreSignal sigINT $
    if not shouldPrompt || unsafePerformIO (putStr url >> readYN)
        then do
            -- open has trouble when opening too many processes
            threadDelay (250*1000)
            (_,_,_,pHandle) <- createProcess (proc "open" [url])
            waitForProcess pHandle >> return (Just "")
        else return (Just "")

readYN :: IO Bool
readYN = do putStr "\n[Y/n]?"
            line <- getLine
            return $ case map toLower line of
                         "yes" -> True
                         "y" -> True
                         "" -> True
                         _ -> False

exportBMs :: [BM] -> String -> IO ()
exportBMs bms outFile = do
        before <- readFile "before.json"
        after <- readFile "after.json"
        writeFile outFile =<< do
            let bef = before ++ "\n[\n"
                aft =  "\n]\n" ++ after
                bms' = map bmToJson bms
            putStrLn "Done!"
            return $ bef ++ intercalate "\n," bms' ++ aft

    where
        bmToJson :: BM -> String
        bmToJson (BM{bmUrl=url,bmTags=tags,bmTitle=_title}) =
            printf "{\"title\":\"%s\",\"charset\":\"UTF-8\",\"tags\":\"%s\",\"type\":\"text/x-moz-place\",\"uri\":\"%s\"}"
            url (intercalate "," $ toList tags) url

-- For importing from a csv file with syntax: `url,title,..,tag`
importBMsFromCSV :: [BM] -> String -> String -> IO ()
importBMsFromCSV bms csvFile outFile = do
        rsavedCSV <- readFile csvFile
        let savedLines = drop 1 . splitOn "\n" $ rsavedCSV
        rsaved <- return . mapMaybe (csvToBM . splitOn ",") $ savedLines
        printStats True rsaved
        length bms `seq` writeFile outFile (showList (bms ++ rsaved))
    where
        csvToBM :: [String] -> Maybe BM
        csvToBM [url,title,_,tag] = Just BM {bmUrl=url,bmTitle=title,bmTags=fromList [tag]}
        csvToBM _ = Nothing

-- For importing from an HTML file with `<a href=".." tags="word[,word]+">`
-- TODO: Don't overwrite dbFile
importBMsFromHtml :: [BM] -> String -> String -> IO ()
importBMsFromHtml _bms dbFile outFile = do
        tags <- return . parseTags =<< readFile dbFile
        writeFile outFile "[BM {bmTags = fromList [\"\"], bmUrl = \"\"}"
        let bms :: [Tag String]
            bms = filter (tagOpenLit "A" (const True)) tags
            bms' = (fromAttrib "TAGS" &&& fromAttrib "HREF") `map` bms
        forM_ (zip [1..] bms') $ \(n,(tag,b)) -> do
            let bm = BM {bmTags=fromList $ splitOn "," tag,bmUrl=b,bmTitle="title"}
            print (n :: Int,(tag,bm))
            tag' <- getTags . bmUrl $ bm
            when (isJust tag') $ do
                let bm' = bm {bmTags=fromList $ toList (bmTags bm) ++ splitOn "," (fromJust tag')}
                appendFile outFile $ "," ++ show bm ++ "\n"
                print bm'
            putStrLn ""
        appendFile outFile "\n]\n"

getTags :: String -> IO (Maybe String)
getTags bm = do
        tag <- getLine
        return $ case tag of
                     "d" -> Nothing
                     "s" -> Just ""
                     "o" -> do void $ unsafePerformIO (open True bm)
                               unsafePerformIO (getTags bm)
                     "" -> unsafePerformIO (getTags bm)
                     _ -> Just tag
