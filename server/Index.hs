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
    "body { margin:0; background:rgb(253,253,253);\
    \       font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif; }\
    \div.topbar {height: 6px; background-color: rgb(96,181,204); }\
    \div.header { padding: 20px 50px; font-size: 30px; }\
    \div.content { padding: 0 40px }\
    \table { width:100%; border-collapse:collapse; margin-bottom: 40px; float: left }\
    \a { text-decoration: none; color:rgb(96,181,204) }\
    \td { padding: 6px 10px; color:rgb(180,180,180) }\
    \tr { border-bottom: solid rgb(245,245,245) 1px }\
    \th { background:rgb(216,221,225); text-align:left;\
    \     padding:6px 10px; font-weight:normal; font-size: 18px; }"

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

    writeS "<div class=\"header\">"
    writeS "<a href='/'>~</a> / "
    when (uri /= "/") $ do
        let path = splitOn "/" uri
        let pathScan = scanl1 (\a b -> a ++ '/' : b) path
        forM_ (zip path pathScan) $ \(p,ps) ->
            unless (null p) $ writeS $ "<a href=\""++ps++"\">"++p++"</a> / "
    writeS "</div><div class=\"content\">"

    entries <- liftIO $ getDirectoryContents d
    dirs    <- liftIO $ filterM (doesDirectoryExist . (d </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (d </>)) entries

    let (elmFiles, otherFiles) = partition (isSuffixOf ".elm") files
    let isHtml5 f = any (flip isSuffixOf f) [".html", ".css", "js"]

    unless (null elmFiles) $ do
        writeS "<table><tr><th>Elm File</th><th>Last Modified</th></tr>"
        forM_ (sort elmFiles ) $ \f -> do
            tm <- liftIO . getModificationTime $ d </> f
            writeS "<tr><td><a href=/debug/"
            writeS $ d </> f
            writeS "><img title=\"Debug mode\" src=/debug.png height=\"12\">"
            writeS "</a>&#8195;<a href="
            writeS f
            writeS ">"
            writeS f
            writeS "</a></td><td>"
            writeS $ formatTime' tm
            writeS "</td></tr>"
        writeS "</table>"

    let dotFile f = null f || head f == '.'
    let keepDir d = d /= "cache" && d /= "build" && not (dotFile d)
    let dirs' = sort $ filter keepDir dirs
    unless (null dirs') $ do
        writeS "<table><tr><th>Directory Name</th></tr>"
        forM_ dirs' $ \d -> do
            writeS "<tr><td><a href='"
            writeS d
            writeS "/'>"
            writeS d
            writeS "</a></tr>"
        writeS "</table>"

    let otherFiles' = sort $ filter (not . dotFile) otherFiles
    unless (null otherFiles') $ do
        writeS "<table><tr><th>File Name</th><th>MIME type</th><th>Last Modified</th></tr>"
        forM_ otherFiles' $ \f -> do
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

