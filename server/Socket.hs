{-# LANGUAGE OverloadedStrings #-}
module Socket where

import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent (threadDelay, forkIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Filesystem.Path.CurrentOS as FP
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WSS
import qualified System.FSNotify.Devel as NDevel
import qualified System.FSNotify as Notify
import System.FilePath
import System.Process

import qualified Generate

fileChangeApp :: [FilePath] -> WS.ServerApp
fileChangeApp watchedFiles pendingConnection =
  do connection <- WS.acceptRequest pendingConnection
     _ <- forkIO $ keepAlive connection
     notifyManager <- liftIO $ Notify.startManager
     updateOnChange notifyManager connection watchedFiles
     Notify.stopManager notifyManager

keepAlive :: WS.Connection -> IO ()
keepAlive connection =
  do WS.sendPing connection $ BSC.pack "ping"
     threadDelay $ 10 * (1000000) -- 10 seconds
     keepAlive connection

updateOnChange :: Notify.WatchManager -> WS.Connection -> [FilePath] -> IO ()
updateOnChange manager connection watchedFiles =
  do _ <- NDevel.treeExtExists manager "." "elm" (sendHotSwap connection watchedFiles)
     threadDelay maxBound

sendHotSwap :: WS.Connection -> [FilePath] -> FP.FilePath -> IO ()
sendHotSwap connection watchedFiles filePath =
  if any (beginningMatch changedFileTokens) watchedTokens
  then
    do result <- liftIO $ Generate.js strChangedFile
       WS.sendTextData connection $ BSC.pack result
  else return ()
  where
    strChangedFile = FP.encodeString filePath
    changedFileTokens = splitDirectories $ normalise strChangedFile
    watchedTokens = map (\x -> splitDirectories $ normalise x) watchedFiles
    beginningMatch l r = all (uncurry (==)) $ zip (reverse l) (reverse r)
