module SideBar.Logs where

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Signal
import Dict exposing (Dict)
import Maybe
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Components exposing (..)

import Styles exposing (..)
import Debugger.Service as DS
import Debugger.Active as Active
import Debugger.RuntimeApi as API
import DataUtils exposing (..)


type alias Model =
  { exprExpansion : ExpansionModel API.ExprTag
  , nodeExpansion : ExpansionModel API.NodeId
  }


initModel : Model
initModel =
  { exprExpansion = emptyExpansionModel
  , nodeExpansion = emptyExpansionModel
  }


type Message
  = CollapseLog LogId Bool
  | UpdateLogs
      { newExprLogs : Dict API.ExprTag API.ValueLog
      , newNodeLogs : Dict API.NodeId API.ValueLog
      }
  | ScrubTo API.FrameIndex
  | NoOp


type LogId
  = NodeLog API.NodeId
  | ExprLog API.ExprTag


update : Message -> Model -> Transaction Message (Model, Maybe API.FrameIndex)
update msg state =
  case msg of
    CollapseLog logId collapsed ->
      case logId of
        NodeLog nodeId ->
          done
            ( { state | nodeExpansion <-
                  collapseLog nodeId collapsed state.nodeExpansion
              }
            , Nothing
            )

        ExprLog exprTag ->
          done
            ( { state | exprExpansion <-
                  collapseLog exprTag collapsed state.exprExpansion
              }
            , Nothing
            )

    UpdateLogs {newExprLogs, newNodeLogs} ->
      done
        ( { state
              | exprExpansion <-
                  updateExpansion state.exprExpansion newExprLogs
              , nodeExpansion <-
                  updateExpansion state.nodeExpansion newNodeLogs
          }
        , Nothing
        )

    ScrubTo frameIdx ->
      done (state, Just frameIdx)

    NoOp ->
      done (state, Nothing)


view : Signal.Address Message -> Int -> Model -> Active.Model -> Html
view addr controlsHeight state activeState =
  let
    curFrame =
      Active.curFrameIdx activeState
  in
    div
      [ style
          [ "overflow-y" => "auto"
          , "overflow-x" => "hidden"
          , "padding" => ("0 " ++ intToPx sidePadding)
          , "position" => "absolute"
          , "bottom" => "10px"
          , "top" => intToPx (controlsHeight + 1)
          , "width" => intToPx (sidebarWidth - 2*sidePadding)
          ]
      ]
      ( if Dict.isEmpty activeState.exprLogs then
          [noLogs]
        else
          [ ul
              [ style
                  [ "list-style" => "none"
                  , "padding-left" => "0"
                  , "color" => "white"
                  ]
              ]
              (activeState.exprLogs
                |> Dict.toList
                |> List.map (\(tag, log) ->
                      viewExprLog
                        addr
                        (Dict.get tag state.exprExpansion |> getMaybe "log not found")
                        (ExprLog tag)
                        curFrame
                        log)
              )
          ]
      )


sidePadding =
  20


viewExprLog : Signal.Address Message -> Bool -> LogId -> API.FrameIndex -> API.ValueLog -> Html
viewExprLog addr collapsed logId frameIdx log =
  let
    clickAttrs =
      [ onClick addr (CollapseLog logId (not collapsed))
      , style ["cursor" => "pointer"]
      ]

    colButton =
      span
        [ style
            ([ "cursor" => "default"
             , "padding-right" => "5px"  
             , "font-size" => "12px"
             ] ++ unselectable)
        ]
        [ text <| if collapsed then "▶" else "▼" ]
  in 
    if collapsed then
      if frameIdx == 0 then
        li
          clickAttrs
          [ colButton
          , text <| logLabel logId
          ]
      else
        li
          clickAttrs
          [ colButton
          , text <| logLabel logId ++ ": "
          , code []
              [ text <|
                  API.prettyPrint
                    (log |> getAtIdx frameIdx |> getMaybe "idx out of range")
              ]
          ]
    else
      li []
        [ span
            clickAttrs
            [ colButton
            , text <| logLabel logId
            ]
        , ul []
            (log |> List.map (\(idx, val) ->
                viewLogItem addr (frameIdx == idx) idx val))
        ]


viewLogItem : Signal.Address Message -> Bool -> API.FrameIndex -> API.JsElmValue -> Html
viewLogItem addr highlighted idx val =
  li
    [ onClick addr (ScrubTo idx)
    , style
        ( [ "cursor" => "pointer" ]
          ++ (if highlighted then ["color" => "yellow"] else [])
        )
    ]
    [ text <| (toString idx) ++ ": "
    , code [] [ text <| API.prettyPrint val ]
    ]


logLabel : LogId -> String
logLabel logId =
  case logId of
    NodeLog id ->
      toString id

    ExprLog tag ->
      tag


-- NO WATCHES


noLogs : Html
noLogs =
  div
    [ style
        [ "font-family" => textTypefaces
        , "color" => "rgb(170, 170, 170)" -- dark grey
        ]
    ]
    [ noLogBlurb ]


-- TODO: update
noLogBlurb : Html
noLogBlurb = Markdown.toHtml """

### <span style="font-size: 12pt"> You don't have any watches! </span>

<span style="font-size: 10pt">
Use [<span style="text-decoration:underline; color: rgb(170, 170, 170)">Debug.watch</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watch)
to show any value. <br>
`watch : String -> a -> a`

<span style="font-size: 10pt">
Use [<span style="text-decoration:underline; color: rgb(170, 170, 170)">Debug.watchSummary</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watchSummary) to show a
summary or subvalue of any value. </span><br>

"""

{- True means collapsed.
(if/when expando-tree views are used to represent elm values, this
type could be extended to model the expansion state of the whole tree.) -}
type alias ExpansionModel comparable =
  Dict comparable Bool


getAtIdx : API.FrameIndex -> API.ValueLog -> Maybe API.JsElmValue
getAtIdx idx log =
  log
    |> List.filter (\(itemIdx, val) -> itemIdx <= idx)
    |> getLast
    |> Maybe.map snd


emptyExpansionModel : ExpansionModel comparable
emptyExpansionModel =
  Dict.empty


updateExpansion : ExpansionModel comparable -> Dict comparable b -> ExpansionModel comparable
updateExpansion expansion items =
  Dict.foldl
    (\tag _ newEM ->
      let expanded =
        case Dict.get tag expansion of
          Just exp ->
            exp
          Nothing ->
            True
      in
        Dict.insert tag expanded newEM
    )
    emptyExpansionModel
    items


collapseLog : comparable -> Bool -> ExpansionModel comparable -> ExpansionModel comparable
collapseLog tag collapsed model =
  Dict.insert tag collapsed model
