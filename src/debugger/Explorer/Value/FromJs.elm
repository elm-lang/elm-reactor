module Explorer.Value.FromJs
    ( ElmValue(..)
    , SeqType(..)
    , toElmValue
    )
    where


import Dict
import Html
import Json.Decode as Json exposing (..)
import Set
import String

import Native.Cast



-- MODEL


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



-- CREATE REIFIED ELM VALUES


toElmValue : Json.Value -> ElmValue
toElmValue jsValue =
  case Json.decodeValue elmValueDecoder jsValue of
    Ok elmValue ->
      elmValue

    Err _ ->
      VBuiltIn "???"


elmValueDecoder : Json.Decoder ElmValue
elmValueDecoder =
  oneOf
    [ map VInt int
    , map VFloat float
    , map VString string
    , objectDecoder
    , map VChar char
    , map VBool bool
    , functionDecoder
    ]


char : Json.Decoder Char
char =
  string `andThen` \str ->
    case String.uncons str of
      Nothing ->
        fail "not a char"

      Just (chr, _) ->
        succeed chr


functionDecoder : Json.Decoder ElmValue
functionDecoder =
  Json.value `andThen` \val ->
    case getFunctionName val of
      Nothing ->
        fail "not a function"

      Just name ->
        succeed (VFunction name)


objectDecoder : Json.Decoder ElmValue
objectDecoder =
  object2 (,) value (dict value) `andThen` (destructObject >> succeed)


destructObject : (Json.Value, Dict.Dict String Json.Value) -> ElmValue
destructObject (rawValue, keyValues) =
  case Dict.get "ctor" keyValues `Maybe.andThen` getString of
    Nothing ->
      case Maybe.map2 (\_ _ -> ()) (Dict.get "id" keyValues) (Dict.get "notify" keyValues) of
        Just _ ->
          VBuiltIn "signal"

        Nothing ->
          VRecord (mapSnd toElmValue (Dict.toList keyValues))

    Just "Set_elm_builtin" ->
      VSeq Set (List.map toElmValue (unsafeCast (Set.toList (unsafeCast rawValue))))

    Just "RBNode_elm_builtin" ->
      VDict (mapBoth toElmValue (unsafeCast (Dict.toList (unsafeCast rawValue))))

    Just "RBEmpty_elm_builtin" ->
      VDict []

    Just "::" ->
      VSeq List (List.map toElmValue (unsafeCast rawValue))

    Just "[]" ->
      VSeq List []

    Just "Element_elm_builtin" ->
      VBuiltIn "element"

    Just "Form_elm_builtin" ->
      VBuiltIn "form"

    Just tag ->
      if String.left 5 tag == "Text:" then
        VBuiltIn "text"

      else if String.left 6 tag == "_Tuple" then
        VSeq Tuple (List.map (toElmValue << snd) (Dict.toList keyValues))

      else
        VSeq (Tag tag) (List.map (toElmValue << snd) (Dict.toList keyValues))


mapSnd : (a -> b) -> List (k, a) -> List (k, b)
mapSnd func list =
  List.map (\(k,v) -> (k, func v)) list


mapBoth : (a -> b) -> List (a, a) -> List (b, b)
mapBoth func list =
  List.map (\(k,v) -> (func k, func v)) list


getString : Json.Value -> Maybe String
getString value =
  case Json.decodeValue string value of
    Ok str ->
      Just str

    Err _ ->
      Nothing



-- NATIVE STUFF


unsafeCast : a -> b
unsafeCast =
  Native.Cast.unsafeCast


getFunctionName : Json.Value -> Maybe String
getFunctionName =
  Native.Cast.getFunctionName


