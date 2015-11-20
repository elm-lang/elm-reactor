module SideBar.ActionLog where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Styles exposing (..)


type Message
    = GoToFrame DM.FrameIndex


view : Signal.Address Message -> DM.ValueLog -> DM.FrameIndex -> Html
view addr actions curFrameIdx =
  ul
    [ style
        [ ("background-color" => "lightgrey")
        , ("margin" => "0")
        ]
    ]
    (List.map (viewAction addr curFrameIdx) actions)


viewAction
    : Signal.Address Message
    -> DM.FrameIndex
    -> (DM.FrameIndex, DM.JsElmValue)
    -> Html
viewAction addr curFrameIdx (frameIdx, value) =
  let
    onThisFrame =
      curFrameIdx == frameIdx
  in
    li
      [ onClick addr (GoToFrame frameIdx)
      , style
          [ "color" => if onThisFrame then "red" else "black"
          , "cursor" => "pointer"
          , "font-family" => "monospace"
          ]
      ]
      [ text (API.prettyPrint value)
      ]
