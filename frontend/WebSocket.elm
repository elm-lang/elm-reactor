module WebSocket where

import Task
import Signal

import Native.WebSocket


type alias URL =
  String


type alias Message =
  String


type WebSocket =
  WebSocket


type SocketEvent
  = Message Message
  | Close


create : URL -> Signal.Address SocketEvent -> Task.Task x WebSocket
create =
  Native.WebSocket.create


send : Message -> WebSocket -> Task.Task x ()
send =
  Native.WebSocket.send
