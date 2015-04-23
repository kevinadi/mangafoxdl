import Codec.Archive.Zip
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Network.HTTP
import Network.URI
import System.Environment
import System.IO
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as B

data Chapter = Chapter
    { chp :: String
    , ttl :: String
    , lnk :: String
    } deriving Show

data Env = Env
    { cbz     :: Archive
    , url     :: String
    , outfile :: String
    , pages   :: [Int]
    } deriving Show

sanitize :: String -> String
sanitize = filter (flip notElem "?:<>|/\\*") . foldr (\a b -> if a=='"' then '\'':b else a:b) []

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

openURL' :: String -> IO String
openURL' url = getResponseBody =<< simpleHTTP (getRequest url)

openURL :: HStream a => String -> IO a
openURL url = let
    uri = case parseURI url of
        Nothing -> error $ ("Invalid URL: " ++ url)
        Just u  -> u
    in getResponseBody =<< simpleHTTP (defaultGETRequest_ uri)    

getpage1 :: Chapter -> IO ()
getpage1 chapter = let
    outfile     = sanitize ((chp chapter) ++ " - " ++ (ttl chapter) ++ ".cbz")
    baseurl     = take =<< subtract 6 . length
    pagecount t = div (length . filter (/="0") . map (fromAttrib "value") . filter (~=="<option>") $ t) 2
    in do
    tags <- fmap parseTags $ openURL (lnk chapter)
    let totalpages = pagecount tags
    putStr $ outfile ++ ": page 1 of " ++ show totalpages
    cbz <- writeimage tags emptyArchive
    evalStateT getpages Env {cbz = cbz, url = baseurl (lnk chapter), outfile = outfile, pages = [2..totalpages]}

getpages :: StateT Env IO ()
getpages = do
    state <- get
    case pages state of
        [] -> liftIO $ B.writeFile (outfile state) (fromArchive (cbz state))
        _  -> do
            liftIO $ putStr $ outfile state ++ ": page " ++ show (head (pages state)) ++ " of " ++ show (last (pages state))
            let lnk = url state ++ show (head (pages state)) ++ ".html"
            tags <- fmap parseTags $ liftIO $ openURL lnk
            cbz <- liftIO $ writeimage tags (cbz state)
            put state { cbz = cbz, pages = tail (pages state) }
            getpages

getimage :: String -> IO B.ByteString
getimage imgsrc = let
    eof x = map (B.index x) [((B.length x)-2), ((B.length x)-1)] == [255,217]
    in do
    img <- openURL imgsrc
    case eof img of
        True  -> do
            putStrLn " OK"
            return img
        False -> do
            putStrLn " Incompete. Retrying..."
            getimage imgsrc

writeimage :: [Tag String] -> Archive -> IO Archive
writeimage tags cbz = let
    imgsrc  = fromAttrib "src" . head . filter (~=="<img id=image>") $ tags
    imgname = last . wordsWhen (=='/') $ imgsrc
    in getimage imgsrc >>= return . flip addEntryToArchive cbz . toEntry imgname 0

main :: IO ()
main = let
    chapters ts tags = case dropWhile (~/= "<a class=tips>") tags of
        [] -> ts
        a  -> let
            b   = dropWhile (~/= "<span class=\"title nowrap\">") a
            lnk = fromAttrib "href" (head a)
            t   = let
                chp = fromTagText (head . drop 1 $ a)
                ttl = case (drop 1 b) of
                    [] -> "untitled"
                    t  -> fromTagText (head t)
                in Chapter { chp = chp, ttl = ttl, lnk = lnk }
            in chapters (t:ts) b

    getchapteridx str cs = case elemIndex True $ map (isInfixOf str . chp) cs of
        Nothing -> error ("Chapter not found")
        Just x  -> x

    getchapterlist m = openURL ("http://mangafox.me/manga/" ++ m) >>= return . chapters [] . parseTags

    in do  --ghci> :main full_metal_panic
    hSetEncoding stdout latin1
    args <- getArgs
    case args of
        [m]   -> getchapterlist m >>= mapM_ (\x -> putStrLn (chp x ++ " - " ++ ttl x))
        [m,x] -> do
            chplist <- getchapterlist m
            let x' = getchapteridx x chplist
            getpage1 (chplist !! x')
        [m,x,y] -> do
            chplist <- getchapterlist m
            let x' = getchapteridx x chplist
            let y' = getchapteridx y chplist
            mapM_ (\i -> getpage1 (chplist !! i)) [x'..y']
        _ -> putStrLn "Usage: mangafoxdl manga_name [from_chapter] [to_chapter]"
