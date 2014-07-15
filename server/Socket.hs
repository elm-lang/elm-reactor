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
import System.Process

import qualified Generate


fileChangeApp :: WS.ServerApp
fileChangeApp pendingConnection = do
      connection <- WS.acceptRequest pendingConnection
      _ <- forkIO $ keepAlive connection
      notifyManager <- liftIO $ Notify.startManager
      updateOnChange notifyManager connection
      Notify.stopManager notifyManager

keepAlive :: WS.Connection -> IO ()
keepAlive connection =
  do WS.sendPing connection $ BSC.pack "ping"
     threadDelay $ 10 * (1000000) -- 10 seconds
     keepAlive connection

updateOnChange :: Notify.WatchManager -> WS.Connection -> IO ()
updateOnChange manager connection =
  do _ <- NDevel.treeExtExists manager "." "elm" (sendHotSwap connection)
     threadDelay maxBound

sendHotSwap :: WS.Connection -> FP.FilePath -> IO ()
sendHotSwap connection filePath =
  do result <- liftIO $ Generate.js $ FP.encodeString filePath
     WS.sendTextData connection $ BSC.pack result
