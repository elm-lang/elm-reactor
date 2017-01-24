{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
  ( lookup
  , cssPath
  , indexPath
  , startPath
  , notFoundPath
  , waitingPath
  , errors
  )
  where

import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import qualified Data.HashMap.Strict as HM
import Language.Haskell.TH (runIO)
import System.FilePath ((</>))

import qualified StaticFiles.Build as Build



-- FILE LOOKUP


type MimeType =
  String


lookup :: FilePath -> Maybe (BS.ByteString, MimeType)
lookup path =
  HM.lookup path dict


dict :: HM.HashMap FilePath (BS.ByteString, MimeType)
dict =
  HM.fromList
    [ faviconPath  ==> (favicon , "image/x-icon")
    , waitingPath  ==> (waiting , "image/gif")
    , indexPath    ==> (index   , "application/javascript")
    , startPath    ==> (start   , "application/javascript")
    , notFoundPath ==> (notFound, "application/javascript")
    , cssPath      ==> (css     , "text/css")
    , codeFontPath ==> (codeFont, "font/ttf")
    , sansFontPath ==> (sansFont, "font/ttf")
    ]


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)


faviconPath :: FilePath
faviconPath =
  "favicon.ico"


waitingPath :: FilePath
waitingPath =
  "_elm/waiting.gif"


indexPath :: FilePath
indexPath =
  "_elm/index.js"


startPath :: FilePath
startPath =
  "_elm/start.js"


notFoundPath :: FilePath
notFoundPath =
  "_elm/notFound.js"


cssPath :: FilePath
cssPath =
  "_elm/styles.css"


codeFontPath :: FilePath
codeFontPath =
  "_elm/source-code-pro.ttf"


sansFontPath :: FilePath
sansFontPath =
  "_elm/source-sans-pro.ttf"



-- ELM PAGES


errors :: BS.ByteString
errors =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "Errors.elm")))


index :: BS.ByteString
index =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "Index.elm")))


start :: BS.ByteString
start =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "Start.elm")))


notFound :: BS.ByteString
notFound =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "NotFound.elm")))



-- CSS / FONTS


css :: BS.ByteString
css =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "styles.css")))


codeFont :: BS.ByteString
codeFont =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "source-code-pro.ttf")))


sansFont :: BS.ByteString
sansFont =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "source-sans-pro.ttf")))



-- IMAGES


favicon :: BS.ByteString
favicon =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "favicon.ico")))


waiting :: BS.ByteString
waiting =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "waiting.gif")))

