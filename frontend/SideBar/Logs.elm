module SideBar.Logs where

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Signal
import Dict
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
      { newExprLogs : Dict.Dict API.ExprTag API.ValueLog
      , newNodeLogs : Dict.Dict API.NodeId API.ValueLog
      }
  | NoOp


type LogId
  = NodeLog API.NodeId
  | ExprLog API.ExprTag


update : Message -> Model -> Transaction Message Model
update msg state =
  case msg of
    CollapseLog logId collapsed ->
      case logId of
        NodeLog nodeId ->
          done
            { state | nodeExpansion <-
                collapseLog nodeId collapsed state.nodeExpansion
            }

        ExprLog exprTag ->
          done
            { state | exprExpansion <-
                collapseLog exprTag collapsed state.exprExpansion
            }

    UpdateLogs {newExprLogs, newNodeLogs} ->
      done
        { state
            | exprExpansion <-
                updateExpansion state.exprExpansion newExprLogs
            , nodeExpansion <-
                updateExpansion state.nodeExpansion newNodeLogs
        }

    NoOp ->
      done state


view : Signal.Address Message -> Int -> Model -> Active.Model -> Html
view addr controlsHeight state activeState =
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
                      log))
        ]
    )


sidePadding =
  20


viewExprLog : Signal.Address Message -> Bool -> LogId -> API.ValueLog -> Html
viewExprLog addr collapsed logId log =
  let
    colButton =
      span
        [ onClick addr (CollapseLog logId (not collapsed))
        , style
            ([ "cursor" => "default"
             , "padding-right" => "5px"
             , "font-size" => "12px"
             ] ++ unselectable)
        ]
        [ text <| if collapsed then "▶" else "▼" ]
  in 
    if collapsed then
      li []
        [ colButton
        , text <| logLabel logId ++ ": " ++ API.prettyPrint (getLast log |> snd)
        ]
    else
      li []
        [ colButton
        , text <| logLabel logId
        , ul []
            (List.map viewLogItem log)
        ]


viewLogItem : (API.FrameIndex, API.JsElmValue) -> Html
viewLogItem (idx, val) =
  li []
    [text <| "frame: " ++ (toString idx) ++ "; val: " ++ (API.prettyPrint val)]


logLabel : LogId -> String
logLabel logId =
  case logId of
    NodeLog id ->
      toString id

    ExprLog tag ->
      tag


-- WATCHES
{-
viewWatch : (String, String) -> Html
viewWatch (name, value) =
  div watchAttributes [viewName name, viewValue value]


watchAttributes : List Attribute
watchAttributes =
  [ style
    [ "color" => colorToCss Color.lightGrey
    , "padding" => "10px 0"
    ]
  ]


viewName : String -> Html
viewName name =
  div nameAttributes [ text name ]


nameAttributes : List Attribute
nameAttributes =
  [ style
    [ "margin" => "10px 0 10px"
    , "font-weight" => "bold"
    , "font-family" => textTypefaces
    ]
  ]


viewValue : String -> Html
viewValue value =
  pre valueAttributes [ text value ]


valueAttributes : List Attribute
valueAttributes =
  [ style
    [ "margin" => "0 0 0 10px" ]
  ]
-}

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
  Dict.Dict comparable Bool


emptyExpansionModel : ExpansionModel comparable
emptyExpansionModel =
  Dict.empty


updateExpansion : ExpansionModel comparable -> Dict.Dict comparable b -> ExpansionModel comparable
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
