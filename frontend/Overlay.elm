module Overlay where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal
import Color

import Styles exposing (..)
import SideBar.Model as Model
import SideBar.Controls as Controls
import SideBar.Watches as Watches


view : Bool -> Model.Model -> Html
view showSwap state =
  div
    []
    [ sidebar showSwap state
    , eventBlocker state.paused
    ]


eventBlocker : Bool -> Html
eventBlocker visible =
  div
    [ id "elm-reactor-event-blocker"
    , style
        [ "position" => "absolute"
        , "top" => "0"
        , "left" => "0"
        , "width" => "100%"
        , "height" => "100%"
        , "display" => if visible then "block" else "none"
        ]
    ]
    []


sidebar : Bool -> Model.Model -> Html
sidebar showSwap state =
  let
    constantStyles =
      [ "background-color" => colorToCss darkGrey
      , "width" => intToPx sidebarWidth
      , "position" => "absolute"
      , "top" => "0"
      , "bottom" => "0"
      , "transition-duration" => "0.3s"
      , "opacity" => "0.97"
      , "z-index" => "1"
      ]
    
    toggleStyles =
      if state.sidebarVisible
      then [ "right" => "0"
           , "width" => intToPx sidebarWidth
           ]
      else [ "width" => "0" ]

    dividerBar =
      div
        [ style
            [ "height" => "1px"
            , "width" => intToPx sidebarWidth
            , "opacity" => "0.3"
            , "background-color" => colorToCss Color.lightGrey
            ]
        ]
        []
  in
    div
      [ id "elm-reactor-side-bar"
      -- done in JS: cancelBubble / stopPropagation on this
      , style (constantStyles ++ toggleStyles)
      ]
      [ toggleTab state
      , Controls.view showSwap state
      , dividerBar
      , Watches.view state.watches
      ]


tabWidth = 25

toggleTab : Model.Model -> Html
toggleTab state =
  div
    [ style
        [ "position" => "absolute"
        , "width" => intToPx tabWidth
        , "height" => "60px"
        , "top" => "50%"
        , "left" => intToPx -tabWidth
        , "border-top-left-radius" => "3px"
        , "border-bottom-left-radius" => "3px"
        , "background" => colorToCss darkGrey
        ]
    , onClick sidebarVisibleMailbox.address (not state.sidebarVisible)
    ]
    []

sidebarVisibleMailbox : Signal.Mailbox Bool
sidebarVisibleMailbox =
  Signal.mailbox True

-- SIGNALS    

main : Signal Html
main =
  -- TODO: move showSwap to the #@$ model
  Signal.map (view showSwap) scene


scene : Signal Model.Model
scene =
  Signal.foldp Model.update Model.startModel aggregateUpdates


aggregateUpdates : Signal Model.Action
aggregateUpdates =
  Signal.mergeMany
    [ Signal.map (always Model.Restart) restartMailbox.signal
    , Signal.map Model.Pause pausedInputMailbox.signal
    , Signal.map Model.TotalEvents eventCounter
    , Signal.map Model.ScrubPosition scrubMailbox.signal
    , Signal.map Model.UpdateWatches watches
    , Signal.map Model.PermitSwap permitSwapMailbox.signal
    , Signal.map Model.SidebarVisible sidebarVisibleMailbox.signal
    , buttonStateMailbox.signal
    ]

-- CONTROL MAILBOXES

permitSwapMailbox =
  Controls.permitSwapMailbox

restartMailbox =
  Controls.restartMailbox

pausedInputMailbox =
  Controls.pausedInputMailbox

scrubMailbox =
  Controls.scrubMailbox

buttonStateMailbox =
  Controls.buttonStateMailbox

-- INCOMING PORTS

port eventCounter : Signal Int

port watches : Signal (List (String, String))

port showSwap : Bool


-- OUTGOING PORTS

port scrubTo : Signal Int
port scrubTo =
    scrubMailbox.signal


port pause : Signal Bool
port pause =
    pausedInputMailbox.signal


port restart : Signal Int
port restart =
    Signal.map (always 0) restartMailbox.signal


port permitSwap : Signal Bool
port permitSwap =
    permitSwapMailbox.signal
