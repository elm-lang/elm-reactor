module Index (elmIndexGenerator) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S
import Data.List
import System.Directory
import System.FilePath
import System.PosixCompat.Files

import Snap.Core
import Snap.Util.FileServe

indexStyle :: BS.ByteString
indexStyle = S.pack
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"

elmIndexGenerator :: MonadSnap m
                      => FilePath   -- ^ Directory to generate index for
                      -> m ()
elmIndexGenerator d = let writeS = writeBS . S.pack in do
    modifyResponse $ setContentType $ S.pack "text/html"

    rq      <- getRequest

    writeS "<style type='text/css'>"
    writeBS indexStyle
    writeS "</style><div class=\"header\">Directory Listing: "
    writeBS (rqURI rq)
    writeS "</div><div class=\"content\">"
    writeS "<table><tr><th>File Name</th><th>Type</th><th>Last Modified"
    writeS "</th></tr>"

    when (rqURI rq /= S.pack "/") $
        writeS "<tr><td><a href='../'>..</a></td><td colspan=2>DIR</td></tr>"

    entries <- liftIO $ getDirectoryContents d
    dirs    <- liftIO $ filterM (doesDirectoryExist . (d </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (d </>)) entries

    forM_ (sort $ filter (not . (`elem` ["..", "."])) dirs) $ \f -> do
        writeS "<tr><td><a href='"
        writeS f
        writeS "/'>"
        writeS f
        writeS "</a></td><td colspan=2>DIR</td></tr>"

    forM_ (sort files) $ \f -> do
        stat <- liftIO $ getFileStatus (d </> f)
        tm   <- liftIO $ formatHttpTime (modificationTime stat)
        writeS "<tr><td><a href='"
        writeS f
        writeS "'>"
        writeS f
        writeS "</a></td><td>"
        writeBS $ fileType defaultMimeTypes f
        writeS "</td><td>"
        writeBS tm
        writeS "</tr>"

    writeS "</table></div>"

