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
import Snap.Core as SC
import Snap.Util.FileServe

indexStyle :: String
indexStyle =
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
elmIndexGenerator directory = do
    let writeS = writeBS . S.pack
    let formatTime' = formatTime defaultTimeLocale "%d %b 20%y, %r"
    modifyResponse $ setContentType (S.pack "text/html")

    let uri = normalise directory

    writeS $ "<style type='text/css'>" ++ indexStyle ++ "</style>"
    writeS $ "<div class=\"topbar\"></div>"

    writeS "<div class=\"header\">"
    writeS "<a href=\"/\">~</a> / "
    when (uri /= ".") $ do
        let path = splitOn "/" uri
        let pathScan = scanl1 (\a b -> a ++ "/" ++ b) path
        forM_ (zip path pathScan) $ \(p,ps) ->
            unless (null p) $ writeS $ "<a href=\"/"++ps++"\">"++p++"</a> / "
    writeS "</div><div class=\"content\">"

    entries <- liftIO $ getDirectoryContents directory
    dirs    <- liftIO $ filterM (doesDirectoryExist . (directory </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (directory </>)) entries

    let (elmFiles, otherFiles) =
            partition (\file -> takeExtension file == ".elm") files

    unless (null elmFiles) $ do
        writeS "<table><tr><th>Elm File</th><th>Last Modified</th></tr>"
        forM_ (sort elmFiles) $ \filePath -> do
            modificationTime <- liftIO . getModificationTime $ directory </> filePath
            writeS $ "<tr><td>"
            writeS $ "<a href=\"/" ++ directory ++ "/" ++ filePath ++ "?debug\">"
            writeS $ "<img title=\"Debug mode\" src=/_reactor/wrench.png height=\"12\">"
            writeS $ "</a>&#8195;<a href=\"/" ++ directory ++ "/" ++ filePath ++ "\">" ++ filePath ++ "</a>"
            writeS $ "</td><td>"
            writeS $ formatTime' modificationTime
            writeS $ "</td></tr>"
        writeS "</table>"

    let dotFile filePath =
            null filePath || head filePath == '.'

    let keepDir directory =
            directory /= "cache"
            && directory /= "build"
            && not (dotFile directory)

    let dirs' = sort (filter keepDir dirs)

    unless (null dirs') $ do
        writeS "<table><tr><th>Directory Name</th></tr>"
        forM_ dirs' $ \directory -> do
            writeS $ "<tr><td>"
            writeS $ "<a href=\"" ++ directory ++ "/\">" ++ directory ++ "</a>"
            writeS $ "</td></tr>"
        writeS "</table>"

    let otherFiles' = sort $ filter (not . dotFile) otherFiles
    unless (null otherFiles') $ do
        writeS "<table><tr><th>File Name</th><th>MIME type</th><th>Last Modified</th></tr>"
        forM_ otherFiles' $ \filePath -> do
            modificationTime <- liftIO . getModificationTime $ directory </> filePath
            writeS $ "<tr>"
            writeS $ "<td><a href=\"" ++ filePath ++ "\">" ++ filePath ++ "</a></td>"
            writeS $ "<td>"
            writeBS $ fileType defaultMimeTypes filePath
            writeS $ "</td><td>"
            writeS $ formatTime' modificationTime
            writeS "</td></tr>"
        writeS "</table></div>"

