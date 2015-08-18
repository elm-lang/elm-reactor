module JsArray where

import Json.Decode as JsDec
import Json.Encode as JsEnc

import Native.JsArray


type JsArray a =
  JsArray


empty : JsArray a
empty =
  Native.JsArray.empty


decode : JsDec.Decoder a -> JsDec.Decoder (JsArray a)
decode =
  Native.JsArray.decode


encode : JsArray JsEnc.Value -> JsEnc.Value
encode =
  Native.JsArray.encode


length : JsArray a -> Int
length =
  Native.JsArray.length


get : Int -> JsArray a -> Maybe a
get idx array =
  case length array of
    0 ->
      Nothing

    arrayLength ->
      if idx < 0 then
        get (length array + idx) array
      else if idx < length array then
        Just (Native.JsArray.get idx array)
      else
        Nothing


split : Int -> JsArray a -> (JsArray a, JsArray a)
split =
  Native.JsArray.split


map : (a -> b) -> JsArray a -> JsArray b
map =
  Native.JsArray.map
