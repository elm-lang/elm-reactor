module Debugger.Reflect (getHtml, toElmValue) where

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
    | VRecord (List (String, ElmValue))
    | VTag String (List ElmValue)
    | VTuple (List ElmValue)
    | VList (List ElmValue)
    | VDict (List (ElmValue, ElmValue))
    | VSet (List ElmValue)
    | VArray (List ElmValue)
    | VFunction String
    | VBuiltIn String


type alias JsElmValue =
  JsEnc.Value


getHtml : JsElmValue -> Html.Html
getHtml =
  Native.Debugger.Reflect.getHtml


toElmValue : a -> Value
toElmValue jsValue =
  decode (jsRepr jsValue)


jsRepr : a -> JsElmValue
jsRepr =
  Native.Debugger.Reflect.jsRepr


decode : JsElmValue -> Value
decode =
  Native.Debugger.Reflect.decode
