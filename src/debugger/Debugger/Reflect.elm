module Debugger.Reflect
    ( ElmValue(..)
    , SeqType(..)
    , getHtml
    , toElmValue
    )
    where


import Dict exposing (Dict)
import Html
import Json.Encode as JsEnc
import Set exposing (Set)

import Native.Debugger.Reflect



type ElmValue
    = VInt Int
    | VFloat Float
    | VChar Char
    | VString String
    | VBool Bool
    | VSeq SeqType (List ElmValue)
    | VRecord (List (String, ElmValue))
    | VDict (List (ElmValue, ElmValue))
    | VFunction String
    | VBuiltIn String


type SeqType
    = Tuple
    | List
    | Set
    | Array
    | Tag String


type alias JsElmValue =
  JsEnc.Value


getHtml : JsElmValue -> Html.Html
getHtml =
  Native.Debugger.Reflect.getHtml


toElmValue : a -> ElmValue
toElmValue jsValue =
  decode (jsRepr jsValue)


jsRepr : a -> JsElmValue
jsRepr =
  Native.Debugger.Reflect.jsRepr


decode : JsElmValue -> ElmValue
decode =
  Native.Debugger.Reflect.decode
