module Explorer.MainPanel where

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


type Message
  = ExprMessage DM.ExprTag Expando.Action
  | NodeMessage DM.NodeId Expando.Action


view : Signal.Address Message -> Active.Model -> Html
view addr state =
  let
    curFrameIdx =
      Active.curFrameIdx state

    exprExpandosWithLabels =
      state.exprLogs
        |> Dict.map (\tag log ->
              log |> DM.logItemForFrameIdx curFrameIdx |> Maybe.map snd)
        |> Dict.toList

    nodeExpandosAtFrame =
      state.nodeLogs
        |> Dict.map (\id log ->
              log |> DM.logItemForFrameIdx curFrameIdx |> Maybe.map snd)

    -- assumes that all salient nodes (foldps and their parents) are subscribed to
    nodeExpandos =
      state.salientNodes.foldps
        |> List.map (\{foldp, parent} ->
              [ ( "Action"
                , parent
                , nodeExpandosAtFrame
                    |> Dict.get parent
                    |> Maybe.withDefault Nothing
                )
              , ( "Model"
                , foldp
                , nodeExpandosAtFrame
                    |> Dict.get foldp
                    |> Maybe.withDefault Nothing
                )
              ])
        |> List.concat
  in
    div
      []
      [ h2 [] [ text "Logs" ]
      , ul
          []
          (List.map
            (\(tag, maybeExpando) ->
              viewValue
                (Signal.forwardTo addr (ExprMessage tag))
                tag
                maybeExpando
            )
            exprExpandosWithLabels)
      , h2 [] [ text "Signals" ]
      , ul
          []
          (List.map
            (\(label, nodeId, maybeExpando) ->
              viewValue
                (Signal.forwardTo addr (NodeMessage nodeId))
                label
                maybeExpando
            )
            nodeExpandos)
      ]


viewValue : Signal.Address Expando.Action -> String -> Maybe Expando -> Html
viewValue addr label maybeExpando =
  div
    []
    [ h3 [] [ text label ]
    , case maybeExpando of
        Just expando ->
          span [style ["font-family" => "monospace"]] [Expando.view addr expando]

        Nothing ->
          p [] [ text "No value at this frame" ]
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
