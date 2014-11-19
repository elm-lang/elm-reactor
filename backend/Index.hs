{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Index (elmIndexGenerator) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S
import Data.List (sort, partition, intercalate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getModificationTime)
import System.FilePath ((</>), takeExtension, splitDirectories)
import Snap.Core (MonadSnap, modifyResponse, setContentType, writeBS)

indexStyle :: S.ByteString
indexStyle =
    S.intercalate "\n"
    [ "body {"
    , "    margin: 0;"
    , "    background: rgb(253,253,253);"
    , "    font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif;"
    , "}"
    , "div.topbar {"
    , "    height: 6px;"
    , "    background-color: rgb(96,181,204);"
    , "}"
    , "div.header {"
    , "    padding: 20px 50px;"
    , "    font-size: 30px;"
    , "}"
    , "div.content { padding: 0 40px }"
    , "table {"
    , "    width:100%;"
    , "    border-collapse: collapse;"
    , "    margin-bottom: 40px;"
    , "    float: left"
    , "}"
    , "a { text-decoration: none; color:rgb(96,181,204) }"
    , "td { padding: 8px 10px; color:rgb(136,136,136) }"
    , "tr { border-bottom: solid rgb(245,245,245) 1px }"
    , "th {"
    , "    text-align: left;"
    , "    padding: 6px 10px;"
    , "    font-weight: normal;"
    , "    font-size: 24px;"
    , "}"
    ]

writeS :: MonadSnap m => FilePath -> m ()
writeS = writeBS . S.pack


replaceChar :: Char -> Char -> String -> String
replaceChar old new string =
    map (\c -> if c == old then new else c) string


makeSafe :: String -> String
makeSafe filePath =
    replaceChar ' ' '+' filePath


writeLink :: MonadSnap m => String -> String -> m ()
writeLink href name =
 do writeBS "<a href=\""
    writeS (makeSafe href)
    writeBS "\">"
    writeS name
    writeBS "</a>"


timeSince :: MonadSnap m => FilePath -> m String
timeSince filePath =
 do modificationTime <- liftIO $ getModificationTime filePath
    currentTime <- liftIO getCurrentTime
    return (showDiff currentTime modificationTime)
 where
    showDiff currentTime modificationTime =
        case diffUTCTime currentTime modificationTime of
          diff
            | diff < minute -> format diff second "second"
            | diff < hour   -> format diff minute "minute"
            | diff < day    -> format diff hour "hour"
            | diff < year   -> format diff day "day"
            | otherwise     -> format diff year "year"

    format diff scale name =
        let t :: Integer
            t = round (diff / scale)
        in
            show t ++ " " ++ name ++ (if t == 1 then "" else "s") ++ " ago"

    second = 1
    minute = 60 * second
    hour = 60 * minute
    day = 24 * hour
    year = 365 * day


elmIndexGenerator :: MonadSnap m => FilePath -> m ()
elmIndexGenerator directory =
 do modifyResponse $ setContentType "text/html; charset=utf-8"

    let title =
          intercalate "/" $
          case splitDirectories directory of
            "." : rest -> "~" : rest
            path -> path

    writeBS "<!DOCTYPE html>\n<html>\n<head>"
    writeBS "<title>"
    writeS title
    writeBS "</title>"
    writeBS "<style type='text/css'>"
    writeBS indexStyle
    writeBS "</style></head><body>"

    writeBS "<div class=\"topbar\"></div>"

    writeBS "<div class=\"header\">"
    writeLink "/" "~"
    writeBS " / "
    case splitDirectories directory of
      _ : pathParts@(_:_) -> do
        let fullPaths = scanl1 (\a b -> a ++ "/" ++ b) pathParts
        forM_ (zip pathParts fullPaths) $ \(part, fullPath) -> do
            writeLink ("/" ++ fullPath) part
            writeBS " / "
      _ -> return ()

    writeBS "</div>"

    writeBS "<div class=\"content\">"

    entries <- liftIO $ getDirectoryContents directory
    allDirs <- liftIO $ filterM (doesDirectoryExist . (directory </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (directory </>)) entries

    let (elmFiles, otherFiles) =
            partition (\file -> takeExtension file == ".elm") files

    let dotFile filePath =
            null filePath || head filePath == '.'

    let keepDir dir =
            dir /= "cache" &&
            dir /= "build" &&
            not (dotFile dir)

    let dirs = sort (filter keepDir allDirs)

    let nonElmFiles = sort $ filter (not . dotFile) otherFiles

    unless (null dirs) $ do
        writeBS "<table><tr><th>Directories</th></tr>"
        forM_ dirs $ \dir -> do
            writeBS "<tr><td>"
            writeLink (dir ++ "/") dir
            writeBS "</td></tr>"
        writeBS "</table>"

    unless (null elmFiles) $ do
        writeBS "<table><tr><th>Elm Files</th><th></th></tr>"
        forM_ (sort elmFiles) $ \filePath -> do
            writeBS "<tr>"

            writeBS "<td>"
            writeLink
                ("/" ++ intercalate "/" (splitDirectories directory) ++ "/" ++ filePath ++ "?debug")
                "<img title=\"Debug mode\" src=/_reactor/wrench.png width=\"12\" height=\"12\">"
            writeBS "&#8195;"
            writeLink filePath filePath
            writeBS "</td>"

            writeBS "<td style=\"text-align:right;\">"
            writeS =<< timeSince (directory </> filePath)
            writeBS "</td>"

            writeBS "</tr>"
        writeBS "</table>"

    unless (null nonElmFiles) $ do
        writeBS "<table>"
        writeBS "<tr><th>Other Files</th><th></th></tr>"
        forM_ nonElmFiles $ \filePath -> do
            writeBS "<tr>"

            writeBS "<td>"
            writeLink filePath filePath 
            writeBS "</td>"

            writeBS "<td style=\"text-align:right;\">"
            writeS =<< timeSince (directory </> filePath)
            writeBS "</td>"

            writeBS "</tr>"
        writeBS "</table>"

    writeBS "</div>"
    writeBS "</body>"
    writeBS "</html>"

