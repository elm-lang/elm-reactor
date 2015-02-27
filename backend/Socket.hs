{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Socket where

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Filesystem.Path.CurrentOS as FP
import qualified Network.WebSockets as WS
import qualified System.FSNotify.Devel as NDevel
import qualified System.FSNotify as Notify

import qualified Compile


fileChangeApp :: FilePath -> WS.ServerApp
fileChangeApp watchedFile pendingConnection =
  do  connection <- WS.acceptRequest pendingConnection
      WS.forkPingThread connection 30
      Notify.withManager $ \notifyManager ->
        do  NDevel.treeExtExists notifyManager "." "elm" (sendHotSwap watchedFile connection)
            forever $ threadDelay maxBound

sendHotSwap :: FilePath -> WS.Connection -> FP.FilePath -> IO ()
sendHotSwap watchedFile connection _ =
  do  result <- liftIO (Compile.toJson watchedFile)
      WS.sendTextData connection (BSC.pack result)
