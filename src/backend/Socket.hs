{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Socket (watchFile) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Char8 as BS
import qualified Network.WebSockets as WS
import qualified System.FSNotify.Devel as Notify
import qualified System.FSNotify as Notify

import qualified Compile


watchFile :: FilePath -> WS.PendingConnection -> IO ()
watchFile watchedFile pendingConnection =
  do  connection <- WS.acceptRequest pendingConnection

      let refresh = compileAndSend watchedFile connection

      refresh

      Notify.withManager $ \mgmt ->
        do  stop <- Notify.treeExtAny mgmt "." ".elm" (const refresh)
            tend connection
            stop


compileAndSend :: FilePath -> WS.Connection -> IO ()
compileAndSend watchedFile connection =
  do  result <- Compile.toJson watchedFile
      WS.sendTextData connection result


tend :: WS.Connection -> IO ()
tend connection =
  let
    pinger :: Integer -> IO a
    pinger n =
      do  threadDelay (5 * 1000 * 1000)
          WS.sendPing connection (BS.pack (show n))
          pinger (n + 1)

    receiver :: IO ()
    receiver =
      do  _ <- WS.receiveDataMessage connection
          receiver

    shutdown :: SomeException -> IO ()
    shutdown _ =
      return ()
  in
    do  _pid <- forkIO (receiver `catch` shutdown)
        pinger 1 `catch` shutdown
