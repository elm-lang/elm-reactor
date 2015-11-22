module Explorer.Value.Expando where

import Dict
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import String

import Explorer.Value.FromJs exposing (ElmValue(..), SeqType(..))
import Utils.Style exposing ((=>))



-- MODEL


type Expando
    = ExInt Int
    | ExFloat Toggle Float
    | ExChar Char
    | ExString Toggle String
    | ExBool Bool
    | ExSeq SeqType Toggle (List Expando)
    | ExRecord Toggle (List (String, Expando))
    | ExDict Toggle (List (Expando, Expando))
    | ExFunction String
    | ExBuiltIn String



-- TOGGLE


type Toggle
    = Show
    | Hide


showIf : Bool -> Toggle
showIf bool =
  if bool then Show else Hide


swap : Toggle -> Toggle
swap toggle =
  case toggle of
    Show ->
      Hide

    Hide ->
      Show



-- INITIALIZE EXPANDOS


init : ElmValue -> Expando
init value =
  case value of
    VInt int ->
      ExInt int

    VFloat float ->
      ExFloat Hide float

    VChar char ->
      ExChar char

    VString string ->
      ExString (showIf (String.length string < 40)) string

    VBool bool ->
      ExBool bool

    VSeq seqType args ->
      ExSeq seqType Hide (List.map init args)

    VRecord fields ->
      ExRecord Hide (List.map (\(key, val) -> (key, init val)) fields)

    VDict fields ->
      ExDict Hide (List.map (\(k,v) -> (init k, init v)) fields)

    VFunction name ->
      ExFunction name

    VBuiltIn name ->
      ExBuiltIn name



-- UPDATE


type Action
    = Swap
    | At Int Action


update : Action -> Expando -> Expando
update action expando =
  case action of
    Swap ->
      updateSwap expando

    At index subAction ->
      updateAt index subAction expando


updateSwap : Expando -> Expando
updateSwap expando =
  case expando of
    ExInt _ ->
      expando

    ExFloat toggle float ->
      ExFloat (swap toggle) float

    ExChar _ ->
      expando

    ExString toggle str ->
      ExString (swap toggle) str

    ExBool _ ->
      expando

    ExSeq seqType toggle args ->
      ExSeq seqType (swap toggle) args

    ExRecord toggle fields ->
      ExRecord (swap toggle) fields

    ExDict toggle fields ->
      ExDict (swap toggle) fields

    ExFunction _ ->
      expando

    ExBuiltIn _ ->
      expando


updateAt : Int -> Action -> Expando -> Expando
updateAt index action expando =
  case expando of
    ExInt _ ->
      expando

    ExFloat _ _ ->
      expando

    ExChar chr ->
      expando

    ExString _ _ ->
      expando

    ExBool _ ->
      expando

    ExSeq seqType toggle args ->
      ExSeq seqType toggle (updateListAt index (update action) args)

    ExRecord toggle fields ->
      ExRecord toggle (updateListAt index (updateSnd action) fields)

    ExDict toggle fields ->
      ExDict toggle (updateListAt index (updateSnd action) fields)

    ExFunction _ ->
      expando

    ExBuiltIn _ ->
      expando


updateListAt : Int -> (a -> a) -> List a -> List a
updateListAt index func list =
  case list of
    [] ->
      []

    x :: xs ->
      if index == 0 then
        func x :: xs

      else
        x :: updateListAt (index - 1) func xs


updateSnd : Action -> (key, Expando) -> (key, Expando)
updateSnd action (key, value) =
  (key, update action value)



-- MERGE VALUES INTO AN EXISTING EXPANDO


merge : ElmValue -> Expando -> Expando
merge newValue existingValue =
  case (newValue, existingValue) of
    (VFloat float, ExFloat toggle _) ->
      ExFloat toggle float

    (VString str, ExString toggle _) ->
      ExString toggle str

    (VSeq seqType elements, ExSeq exSeqType toggle exElements) ->
      ExSeq seqType toggle <|
        case (seqType, exSeqType) of
          (Tag tag, Tag exTag) ->
            if tag == exTag then
              List.map2 merge elements exElements

            else
              List.map init elements

          _ ->
            mergeList elements exElements

    (VRecord fields, ExRecord toggle exFields) ->
      ExRecord toggle (mergeFields fields exFields)

    (VDict fields, ExDict toggle _) ->
      -- TODO: be more clever here
      init newValue

    (_, _) ->
      init newValue


mergeList : List ElmValue -> List Expando -> List Expando
mergeList values expandos =
  case (values, expandos) of
    (v :: vs, e :: es) ->
      merge v e :: mergeList vs es

    (_, []) ->
      List.map init values

    ([], _) ->
      []


mergeFields
    : List (String, ElmValue)
    -> List (String, Expando)
    -> List (String, Expando)
mergeFields fields exFields =
  let
    exFieldsDict =
      Dict.fromList exFields

    mergeField (key, value) =
      case Dict.get key exFieldsDict of
        Nothing ->
          (key, init value)

        Just exValue ->
          (key, merge value exValue)
  in
    List.map mergeField fields



-- VIEW


view : Signal.Address Action -> Expando -> Html
view address expando =
  case expando of
    ExInt int ->
      literal (toString int)

    ExFloat toggle float ->
      case toggle of
        Show ->
          literal (toString float)

        Hide ->
          let
            truncated =
              toFloat (round (100 * float)) / 100
          in
            if truncated == float then
              literal (toString truncated)

            else
              span []
                [ literal (toString truncated)
                , ellipsis address
                ]

    ExChar chr ->
      literal (toString chr)

    ExString toggle str ->
      case toggle of
        Show ->
          literal (toString str)

        Hide ->
          let
            len =
              String.length str
          in
            if len > 40 then
              span []
                [ literal ("\"" ++ String.left (min 40 (len - 10)) str)
                , ellipsis address
                ]

            else
              literal (toString str)

    ExBool bool ->
      literal (toString bool)

    ExSeq seqType toggle args ->
      div
        []
        [ strong [] [ text "TODO: don't just toString this. " ]
        , text <| toString (seqType, toggle, args)
        ]

    ExRecord toggle fields ->
      case toggle of
        Show ->
          text "{ ... more stuff ... }"

        Hide ->
          span []
            [ text "{ "
            , ellipsis address
            , text " }"
            ]

    ExDict toggle fields ->
      Debug.crash "TODO"

    ExFunction name ->
      text <|
        if String.isEmpty name then
          "<function>"

        else
          "<function: " ++ name ++ ">"

    ExBuiltIn name ->
      text ("<" ++ name ++ ">")



-- LITERAL VIEW


literal : String -> Html
literal str =
  span [style ["color" => literalColor]] [text str]


ellipsis : Signal.Address Action -> Html
ellipsis address =
  swapper address "..."


literalColor : String
literalColor =
  "green"


swapper : Signal.Address Action -> String -> Html
swapper address str =
  span [onClick address Swap] [text str]


