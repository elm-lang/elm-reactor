{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Socket where

import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BSC
import qualified Network.WebSockets as WS
import qualified System.FSNotify.Devel as NDevel
import qualified System.FSNotify as Notify

import qualified Compile


fileChangeApp :: FilePath -> WS.ServerApp
fileChangeApp watchedFile pendingConnection =
  do  connection <- WS.acceptRequest pendingConnection
      Notify.withManager $ \notifyManager ->
        do  putStrLn $ "swapping initialized for " ++ watchedFile
            _ <- NDevel.treeExtExists notifyManager "." "elm" (sendHotSwap watchedFile connection)
            -- if we don't keep the thread alive, the above file watcher will die
            loopForever


sendHotSwap :: FilePath -> WS.Connection -> FilePath -> IO ()
sendHotSwap watchedFile connection _ =
  do  result <- liftIO (Compile.toJson watchedFile)
      putStrLn $ "swapped " ++ watchedFile
      WS.sendTextData connection (BSC.pack result)


loopForever :: IO ()
loopForever =
  do  threadDelay (10 * 1000000) -- 10 seconds
      loopForever
