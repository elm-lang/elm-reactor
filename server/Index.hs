{-# OPTIONS_GHC -W #-}
{-# LANGUAGE OverloadedStrings #-}
module Index (elmIndexGenerator) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.List.Split (splitOn)
import Data.Time.Format (formatTime)
import System.Directory as Dir
import System.FilePath as FP
import System.Locale (defaultTimeLocale)

import Snap.Core as Snap

indexStyle :: S.ByteString
indexStyle =
    S.intercalate "\n"
    [ "body {"
    , "    margin:0; background:rgb(253,253,253);"
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
    , "td { padding: 6px 10px; color:rgb(180,180,180) }"
    , "tr { border-bottom: solid rgb(245,245,245) 1px }"
    , "th {"
    , "    background: rgb(216,221,225);"
    , "    text-align: left;"
    , "    padding: 6px 10px;"
    , "    font-weight: normal;"
    , "    font-size: 18px;"
    , "}"
    ]

writeS :: MonadSnap m => FilePath -> m ()
writeS = writeBS . S.pack


makeSafe :: String -> String
makeSafe path =
    map (\c -> if c == ' ' then '+' else c) path


writeLink :: MonadSnap m => String -> String -> m ()
writeLink href name =
 do writeBS "<a href=\""
    writeS (makeSafe href)
    writeBS "\">"
    writeS name
    writeBS "</a>"


elmIndexGenerator :: MonadSnap m => FilePath -> m ()
elmIndexGenerator directory =
 do modifyResponse $ setContentType "text/html; charset=utf-8"

    writeBS "<!DOCTYPE html>\n<html>\n<head>"
    writeBS "<title>"
    writeS directory
    writeBS "</title>"
    writeBS "<style type='text/css'>"
    writeBS indexStyle
    writeBS "</style></head><body>"

    let formatTime' = formatTime defaultTimeLocale "%d %b 20%y, %r"

    writeBS "<div class=\"topbar\"></div>"

    writeBS "<div class=\"header\">"
    writeLink "/" "~"
    writeBS " / "
    case splitOn "/" directory of
      _ : pathParts@(_:_) -> do
        let fullPaths = scanl1 (\a b -> a ++ "/" ++ b) pathParts
        forM_ (zip pathParts fullPaths) $ \(part, fullPath) -> do
            writeLink ("/" ++ fullPath) part
            writeBS " / "
      _ -> return ()

    writeBS "</div>"

    writeBS "<div class=\"content\">"

    entries <- liftIO $ getDirectoryContents directory
    dirs    <- liftIO $ filterM (doesDirectoryExist . (directory </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (directory </>)) entries

    let (elmFiles, otherFiles) =
            partition (\file -> takeExtension file == ".elm") files

    unless (null elmFiles) $ do
        writeBS "<table><tr><th>Elm File</th><th>Last Modified</th></tr>"
        forM_ (sort elmFiles) $ \filePath -> do
            modificationTime <- liftIO . getModificationTime $ directory </> filePath
            writeBS "<tr><td>"
            writeLink
                ("/" ++ directory ++ "/" ++ filePath ++ "?debug")
                "<img title=\"Debug mode\" src=/_reactor/wrench.png width=\"12\" height=\"12\">"
            writeBS "&#8195;"
            writeLink filePath filePath
            writeBS "</td><td>"
            writeS $ formatTime' modificationTime
            writeBS "</td></tr>"
        writeBS "</table>"

    let dotFile filePath =
            null filePath || head filePath == '.'

    let keepDir directory =
            directory /= "cache"
            && directory /= "build"
            && not (dotFile directory)

    let dirs' = sort (filter keepDir dirs)

    unless (null dirs') $ do
        writeBS "<table><tr><th>Directory Name</th></tr>"
        forM_ dirs' $ \directory -> do
            writeBS "<tr><td>"
            writeLink (directory ++ "/") directory
            writeBS "</td></tr>"
        writeBS "</table>"

    let otherFiles' = sort $ filter (not . dotFile) otherFiles
    unless (null otherFiles') $ do
        writeBS "<table>"
        writeBS "<tr><th>File Name</th><th>Last Modified</th></tr>"
        forM_ otherFiles' $ \filePath -> do
            modificationTime <- liftIO . getModificationTime $ directory </> filePath
            writeBS "<tr>"

            writeBS "<td>"
            writeLink filePath filePath 
            writeBS "</td>"

            writeBS "<td>"
            writeS $ formatTime' modificationTime
            writeBS "</td>"

            writeBS "</tr>"
        writeBS "</table>"

    writeBS "</div>"
    writeBS "</body>"
    writeBS "</html>"

