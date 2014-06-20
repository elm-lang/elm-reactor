module Index (elmIndexGenerator) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.List.Split (splitOn)
import Data.Time.Format (formatTime)
import System.Directory
import System.FilePath
import System.Locale (defaultTimeLocale)

import Snap.Core
import Snap.Util.FileServe

indexStyle :: BS.ByteString
indexStyle = S.pack
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.topbar {height: 6px; background-color: rgb(90,99,120); }\
    \div.intro  { padding: 20px 50px 0px; font-size: 14px; color:#888 }\
    \div.header { padding: 0px 50px 20px; font-size: 24px; color:rgb(78,78,78) }\
    \div.content { padding: 0 40px }\
    \table { width:100%; border-collapse:collapse; margin-bottom: 40px; float: left }\
    \table.half { width:50% }\
    \a { text-decoration: none; color:rgb(90,99,120) }\
    \td { padding: 6px 0px; color:rgb(78,78,78) }\
    \tr { border-bottom: solid rgb(216, 221, 225) 1px }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:3px 10px; font-weight:normal; }"

elmIndexGenerator :: MonadSnap m
                      => FilePath   -- ^ Directory to generate index for
                      -> m ()
elmIndexGenerator d = do
    let writeS = writeBS . S.pack
    let formatTime' = formatTime defaultTimeLocale "%d %b 20%y, %r"
    modifyResponse $ setContentType $ S.pack "text/html"

    rq      <- getRequest
    let uri = S.unpack $ rqURI rq

    writeS "<style type='text/css'>"
    writeBS indexStyle
    writeS "</style><div class=\"topbar\"></div>"
    writeS "<div class=\"intro\">Directory Listing</div>"
    writeS "<div class=\"header\">"
    writeS "<a href='/'>~</a>/"
    when (uri /= "/") $ do
        let path = splitOn "/" uri
        let pathScan = scanr1 (\a b -> a ++ ('/':b)) path
        mapM_ (\(p,ps) -> unless (null p) $ writeS $ "<a href='"++ps++"'>"++p++"</a>/") (zip path pathScan)
    writeS "</div><div class=\"content\">"

    entries <- liftIO $ getDirectoryContents d
    dirs    <- liftIO $ filterM (doesDirectoryExist . (d </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (d </>)) entries

    let (elmFiles, notElmFiles) = partition (isSuffixOf ".elm") files
    let isHtml5 f = any (flip isSuffixOf f) [".html", ".css", "js"]
    let (html5Files, otherFiles) = partition isHtml5 notElmFiles

    unless (null elmFiles) $ writeS "<table><tr><th>Elm File</th><th>Last Modified</th></tr>"
    forM_ (sort elmFiles ) $ \f -> do
        tm <- liftIO . getModificationTime $ d </> f
        writeS "<tr><td><a href='"
        writeS f
        writeS "'>"
        writeS f
        writeS "</a></td><td>"
        writeS $ formatTime' tm
        writeS "</td></tr>"
    unless (null elmFiles) $ writeS "</table>"

    let dotFile f = null f || head f == '.'
    let keepDir d = d /= "cache" && d /= "build" && not (dotFile d)
    writeS "<table class=\"half\"><tr><th>Directory Name</th></tr>"
    forM_ (sort $ filter keepDir dirs) $ \d -> do
        writeS "<tr><td><a href='"
        writeS d
        writeS "/'>"
        writeS d
        writeS "</a></tr>"
    writeS "</table>"

    writeS "<table class=\"half\"><tr><th>HTML5 File</th></tr>"
    forM_ (sort html5Files) $ \d -> do
        writeS "<tr><td><a href='"
        writeS d
        writeS "/'>"
        writeS d
        writeS "</a></tr>"
    writeS "</table>"

    writeS "<table><tr><th>File Name</th><th>MIME type</th><th>Last Modified</th></tr>"
    forM_ (sort $ filter (not . dotFile) otherFiles) $ \f -> do
        tm <- liftIO . getModificationTime $ d </> f
        writeS "<tr><td><a href='"
        writeS f
        writeS "'>"
        writeS f
        writeS "</a></td><td>"
        writeBS $ fileType defaultMimeTypes f
        writeS "</td><td>"
        writeS $ formatTime' tm
        writeS "</tr>"

    writeS "</table></div>"

