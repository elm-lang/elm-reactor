module Utils where

import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import qualified Paths_elm_server as This

-- |The absolute path to a data file
getDataFile :: FilePath -> IO FilePath
getDataFile name = do
  path <- This.getDataFileName name
  exists <- doesFileExist path
  if exists
    then return path
    else do
      env <- getEnv "ELM_HOME"
      return (env </> "server" </> name)

-- Thanks http://stackoverflow.com/a/4981265
-- wordsWhen (','==) "file1,file2" => ["file1", "file2"]
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen predicate str = 
    case dropWhile predicate str of
        "" -> []
        shorter -> w : wordsWhen predicate noMatch
            where (w, noMatch) = break predicate shorter
