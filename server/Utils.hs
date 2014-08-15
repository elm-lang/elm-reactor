module Utils where

import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import qualified Paths_elm_server as This

-- |The absolute path to a data file
getDataFile :: FilePath -> IO FilePath
getDataFile name = do
  path <- This.getDataFileName name
  exists <- doesFileExist path
  if exists
    then return path
    else do
      environment <- tryIOError (getEnv "ELM_HOME")
      case environment of
        Right env -> return (env </> name)
        Left _ ->
          fail $ unlines
            [ "Unable to find the ELM_HOME environment variable when searching"
            , "for the " ++ name ++ " file."
            , ""
            , "If you installed Elm Platform with the Mac or Windows installer, it looks like"
            , "ELM_HOME was not set automatically. Look up how to set environment variables"
            , "on your platform and set ELM_HOME to the directory that contains Elm's static"
            , "files:"
            , ""
            , "  * On Mac it is /usr/local/share/elm"
            , "  * On Windows it is one of the following:"
            , "      C:/Program Files/Elm Platform/0.13/share"
            , "      C:/Program Files (x86)/Elm Platform/0.13/share"
            , ""
            , "If it seems like a more complex issue, please report it here:"
            , "    <https://github.com/elm-lang/elm-platform/issues>"
            ]
