module SessionRecord
    ( decodeSessionRecord
    , encodeSessionRecord
    ) where

import Json.Encode as JsEnc
import Json.Decode as JsDec exposing ((:=))

import JsArray exposing (JsArray)

import Debugger.Model exposing (..)


decodeEvent : JsDec.Decoder Event
decodeEvent =
  JsDec.object3
    (\value nodeId time ->
      { value = value, nodeId = nodeId, time = time })
    ("value" := JsDec.value)
    ("nodeId" := JsDec.int)
    ("time" := JsDec.float)


encodeEvent : Event -> JsEnc.Value
encodeEvent event =
  JsEnc.object
    [ ("value", event.value)
    , ("nodeId", JsEnc.int event.nodeId)
    , ("time", JsEnc.float event.time)
    ]


decodeSessionRecord : JsDec.Decoder SessionRecord
decodeSessionRecord =
  JsDec.object2
    (\name history ->
      { moduleName = name
      , inputHistory = history
      })
    ("moduleName" := JsDec.string)
    ("inputHistory" := JsArray.decode decodeEvent)


encodeSessionRecord : SessionRecord -> JsEnc.Value
encodeSessionRecord record =
  JsEnc.object
    [ ( "moduleName", JsEnc.string record.moduleName )
    , ( "inputHistory"
      , record.inputHistory |> JsArray.map encodeEvent |> JsArray.encode
      )
    ]
