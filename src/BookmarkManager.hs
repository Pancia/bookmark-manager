module BookmarkManager where

import Prelude hiding (appendFile, readFile, writeFile, showList, print, getLine, putStr, putStrLn)

import Control.Arrow ((&&&))
import Control.Conditional hiding (unless, when)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Char (toLower)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (Set, difference, fromList, toList)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Printf

import Control.Lens
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Response)
import qualified Network.Wreq as Network

import System.IO.Unsafe

import Utils

type TAG = String
type URL = String
type TITLE = String
data BM = BM { bmTags :: Set TAG
             , bmUrl :: URL
             , bmTitle :: TITLE
             } deriving (Show, Read, Eq, Ord)
dfltTitle :: String
dfltTitle = "autogen_title"
mkBM :: (MonadIO m, MockIO m) => [TAG] -> URL -> m BM
mkBM tags url = do let b = BM (fromList tags) url dfltTitle
                   liftIO $ getBmTitle b
printBM :: (MockIO m) => BM -> m ()
printBM = print . (bmTags &&& bmUrl)

getBmTitle :: (MonadIO m, MockIO m) => BM -> m BM
getBmTitle bm@(BM{bmUrl=url,bmTitle=title})
        | title == dfltTitle = do
            let opts = Network.defaults & Network.redirects .~ 1
                handler :: HttpException -> IO (Maybe (Response ByteString))
                handler = const $ return Nothing
            r <- liftIO $ liftM Just (Network.getWith opts url) `catch` handler
            if isNothing r
                then return bm{bmTitle=url}
                else do let str = fromJust r ^. Network.responseBody
                            title' = getTitleFromHtml $ unpack str
                            bm' = bm{bmTitle=either (const url) id title'}
                        putStrLn $ "\n" ++ show bm' ++ "\n"
                        return bm'
        | otherwise = return bm

getTitleFromHtml :: String -> Either [Tag String] TITLE
getTitleFromHtml html = let tags = parseTags html
                            hasTitle = not . null $ filter (tagOpenLit "title" (const True)) tags
                            titles = getTagContent "title" (const True) tags
                        in if hasTitle && not (null titles)
                               then Right . fromTagText . head $ titles
                               else Left tags

repl :: (MonadIO m, MockIO m) => Options -> [BM] -> m ()
repl opts_ bms = do
    putStr "[ EXIT | ADD | STATS | IMPORT | EXPORT | OPEN | DELETE | UPDATE | tags+ ]\n>$"
    (cmd:tags) <- liftM (splitOn " ") getLine
    let shouldPrompt = case last cmd of
                           '?' -> True
                           '!' -> False
                           _ -> optPrompt opts_
        dbFile = optDbFile opts_
        opts = opts_ {optPrompt=shouldPrompt}
    cond [(cmd == "",
          repl opts bms)
         ,(cmd == "EXIT",
          putStrLn "Shutting Down")
         ,("STATS" `isPrefixOf` cmd,
          do if not (null tags)
                 then printStats False (findByTags bms tags)
                 else printStats False bms
             repl opts bms)
         ,("ADD" `isPrefixOf` cmd,
          do bms' <- if null tags
                         then do bm <- getBM "" []
                                 addBM dbFile bms bm
                         else do let [url,tags'] = splitOn "#" (head tags)
                                 bm <- getBM url (splitOn "," tags')
                                 addBM dbFile bms bm
             repl opts bms')
         ,("IMPORT" `isPrefixOf` cmd,
          do putStrLn "importing"
             repl opts bms)
         ,("EXPORT" `isPrefixOf` cmd,
          do putStrLn "exporting"
             repl opts bms)
         ,("OPEN" `isPrefixOf` cmd,
          do let bms' = findByTags bms tags
             putStrLn $ "opening " ++ show (length bms') ++ " links"
             mapM_ (openURL shouldPrompt . bmUrl) bms'
             repl opts bms)
         ,("DELETE" `isPrefixOf` cmd,
          do bms' <- deleteBMs opts tags bms
             repl opts bms')
         ,("UPDATE" `isPrefixOf` cmd,
          do bms' <- updateBMs opts bms [] []
             repl opts bms')
         ,(otherwise,
          do let bms' = findByTags bms (cmd:tags)
             putStr . showList $ bms'
             putStrLn $ "Num of found items: " ++ show (length bms') ++ "\n"
             repl opts bms)]
    where
        getBM :: (MonadIO m, MockIO m) => String -> [String] -> m BM
        getBM url tags
            | null url =
                do putStr "[url#tags]>$"
                   input <- getLine
                   let [url',tags'] = splitOn "#" input
                   bm <- mkBM (splitOn "," tags') url'
                   getBmTitle bm
            | otherwise =
                mkBM tags url >>= getBmTitle

addBM :: (MockIO m) => String -> [BM] -> BM -> m [BM]
addBM dbFile bms bm = do
    length bms `seq` writeFile dbFile (showList $ bm:bms)
    return (bm:bms)

printStats :: (MockIO m) => Bool -> [BM] -> m ()
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
          frequencies :: (Ord a) => [a] -> [(a, Int)]
          frequencies = map (head &&& length) . group . sort

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

updateBMs :: (MockIO m) => Options -> [BM] -> [String] -> [String] -> m [BM]
updateBMs opts bms oldTags newTags = do
        let dbFile = optDbFile opts
            shouldPrompt = optPrompt opts
        newBms <- forM bms (maybeReplace shouldPrompt)
        length bms `seq` writeFile dbFile (showList newBms)
        return newBms
    where
        maybeReplace :: (MockIO m) => Bool -> BM -> m BM
        maybeReplace shouldPrompt bm =
            if (bm =?< oldTags)
                && (not shouldPrompt
                   || unsafePerformIO (readYN $ show bm))
                then return $ replace bm oldTags newTags
                else return bm
        replace :: BM -> [String] -> [String] -> BM
        replace bm@(BM{bmTags=tags}) old new =
            let tags' = filter (not . (`elem` old)) (toList tags)
            in bm {bmTags=fromList $ tags' ++ new}

deleteBMs :: (MonadIO m, MockIO m) => Options -> [String] -> [BM] -> m [BM]
deleteBMs opts tags bms = do
        let dbFile = optDbFile opts
            shouldPrompt = optPrompt opts
            bmsByTag = findByTags bms tags
        tagsToDelete <- if shouldPrompt
                            then sequence $ mapMaybe promptDelete bmsByTag
                            else return bmsByTag
        let bms' = difference (fromList bms) (fromList tagsToDelete)
        length bms `seq` writeFile dbFile . showList $ toList bms'
        return $ toList bms'
    where
        promptDelete :: (MockIO m) => BM -> Maybe (m BM)
        promptDelete bm = do let prompt = show bm ++ "\nDelete the above BM?"
                             if unsafePerformIO (readYN prompt)
                                 then Just $ return bm
                                 else Nothing

showList :: (Show a) => [a] -> String
showList list = "[" ++ intercalate "\n," (map show list) ++ "]\n"

openURL :: (MonadIO m, MockIO m) => Bool -> URL -> m ()
openURL shouldPrompt url =
    if shouldPrompt
        then do shouldOpenUrl <- readYN url
                when shouldOpenUrl $ open url True
        else open url False

readYN :: (MockIO m) => String -> m Bool
readYN prompt = do
        putStrLn $ prompt ++ "\n[Y/n]$"
        line <- getLine
        return $ map toLower line `elem` ["yes","y",""]

-- TODO: Test with a bm with all sorts of chars
exportBMs :: (MockIO m) => [BM] -> String -> m ()
exportBMs bms dbFile = do
        before <- readFile "before.json"
        after <- readFile "after.json"
        writeFile dbFile =<< do
            let bef = before ++ "\n[\n"
                aft = "\n]\n" ++ after
                bms' = map bmToJson bms
            putStrLn $ "Exported to (" ++ dbFile ++ ")"
            return $ bef ++ intercalate "\n," bms' ++ aft
    where
        bmToJson :: BM -> String
        bmToJson (BM{bmUrl=url,bmTags=tags,bmTitle=title}) =
            printf "{\"title\":\"%s\",\"charset\":\"UTF-8\",\"tags\":\"%s\",\"type\":\"text/x-moz-place\",\"uri\":\"%s\"}"
            (filter (`elem` validChars) title)
            (intercalate "," $ toList tags) url
        validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
                     ++ " -_=+;:|!@#$%^&*()[]{}/?<>,."

-- For importing from a csv file with syntax: `url,title,..,tag`
importBMsFromCSV :: (MockIO m) => [BM] -> String -> String -> m ()
importBMsFromCSV bms csvFile dbFile = do
        rsavedCSV <- readFile csvFile
        let savedLines = drop 1 . splitOn "\n" $ rsavedCSV
        rsaved <- return . mapMaybe (csvToTuple . splitOn ",") $ savedLines
        printStats True rsaved
        length bms `seq` writeFile dbFile (showList (bms ++ rsaved))
    where
        csvToTuple :: [String] -> Maybe BM
        csvToTuple [url,title,_,tag] = do
            let iTag = "imported_csv"
            Just $ BM (fromList [iTag,tag]) url title
        csvToTuple _ = Nothing

-- For importing from an HTML file with `<a href=".." tags="tag,tags">`
importBMsFromHtml :: (MonadIO m, MockIO m) => [BM] -> String -> String -> m ()
importBMsFromHtml dbBms dbFile htmlFile = do
        tags <- return . parseTags =<< readFile htmlFile
        let iTag = "imported_html"
            bms :: [Tag String]
            bms = filter (tagOpenLit "A" (const True)) tags
            tagsNurls = ((splitOn "," . fromAttrib "TAGS") &&& fromAttrib "HREF")
                        `map` bms
        imported <- forM tagsNurls $
            \(tags',url) -> do bm <- mkBM (iTag:tags') url
                               getBmTitle bm
        writeFile dbFile (showList $ dbBms ++ imported)
