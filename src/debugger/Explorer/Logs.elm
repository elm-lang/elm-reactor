module Explorer.Logs where

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Markdown

import Debugger.Active as Active
import Debugger.Model as DM
import Debugger.RuntimeApi as API
import Explorer.Value.Expando as Expando exposing (Expando)
import Explorer.Value.FromJs as FromJs
import Utils.Helpers exposing (last, unsafe)
import Utils.Style exposing ((=>), textTypefaces, unselectable)


type alias Model =
  { exprExpandos : Dict DM.ExprTag Expando
  , nodeExpandos : Dict DM.NodeId Expando
  }


initModel : Model
initModel =
  { exprExpandos = Dict.empty
  , nodeExpandos = Dict.empty
  }


type Message
  = ExprMessage DM.ExprTag Expando.Action
  | NodeMessage DM.NodeId Expando.Action


update : Message -> Model -> Model
update msg state =
  let d = Debug.log "Logs.update msg" msg
  in case msg of
    ExprMessage tag action ->
      { state | exprExpandos =
          Dict.update tag (Maybe.map (Expando.update action)) state.exprExpandos
      }

    NodeMessage id action ->
      { state | nodeExpandos =
          Dict.update id (Maybe.map (Expando.update action)) state.nodeExpandos
      }


view : Signal.Address Message -> Model -> Active.Model -> Html
view addr state activeState =
  let
    curFrameIdx =
      Active.curFrameIdx activeState

    -- TODO: dedup, optimize
    curExprValues =
      activeState.exprLogs
      |> Dict.toList
      |> List.filterMap
          (\(tag, valueLog) ->
            (getAtIdx curFrameIdx valueLog
                |> Maybe.map (\value -> (tag, value))))
      |> Dict.fromList
        

    curNodeValues : Dict DM.NodeId DM.JsElmValue
    curNodeValues =
      activeState.nodeLogs
      |> Dict.toList
      |> List.filterMap
          (\(id, valueLog) ->
            getAtIdx curFrameIdx valueLog
              |> Maybe.map (\value -> (id, value)))
      |> Dict.fromList
        

    merge : Dict comparable Expando -> Dict comparable DM.JsElmValue -> Dict comparable Expando
    merge expandos values =
      values
      |> Dict.map
          (\key value ->
            let
              elmValue =
                FromJs.toElmValue value
            in
              case Dict.get key expandos of
                Just prevExpando ->
                  Expando.merge elmValue prevExpando

                Nothing ->
                  Expando.init elmValue)

    curExprExpandos =
      merge state.exprExpandos curExprValues
        |> Dict.toList

    curNodeExpandos =
      merge state.nodeExpandos curNodeValues
        |> Dict.toList

    labelForNode nodeId =
      activeState.salientNodes.foldps
        |> List.filterMap (\{parent, foldp} ->
            if nodeId == parent then
              Just "Action"
            else if nodeId == foldp then
              Just "Model"
            else
              Nothing)
        |> List.head
        |> unsafe "not a salient node"
  in
    div
      []
      [ h2 [] [ text "Logs" ]
      , ul
          []
          (List.map
            (\(tag, expando) ->
              viewValue
                (Signal.forwardTo addr (ExprMessage tag))
                tag
                expando)
            curExprExpandos)
      , h2 [] [ text "Signals" ]
      , ul
          []
          (List.map
            (\(id, expando) ->
              viewValue
                (Signal.forwardTo addr (NodeMessage id))
                (labelForNode id)
                expando)
            curNodeExpandos)
      ]


viewValue : Signal.Address Expando.Action -> String -> Expando -> Html
viewValue addr label expando =
  div
    []
    [ h3 [] [ text label ]
    , Expando.view addr expando
    ]


-- NO WATCHES


noLogs : Html
noLogs =
  div
    [ style
        [ "font-family" => textTypefaces ]
    ]
    [ noLogBlurb ]


-- TODO: update
noLogBlurb : Html
noLogBlurb = Markdown.toHtml """

### <span style="font-size: 12pt"> You don't have any logs! </span>

<span style="font-size: 10pt">
Use [<span style="text-decoration:underline">Debug.watch</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watch)
to show any value. <br>
`watch : String -> a -> a`

<span style="font-size: 10pt">
Use [<span style="text-decoration:underline">Debug.watchSummary</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watchSummary) to show a
summary or subvalue of any value. </span><br>

"""


getAtIdx : DM.FrameIndex -> DM.ValueLog -> Maybe DM.JsElmValue
getAtIdx idx log =
  log
    |> List.filter (\(itemIdx, val) -> itemIdx <= idx)
    |> last
    |> Maybe.map snd
