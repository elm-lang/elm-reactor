module Overlay where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal
import Window

import Styles exposing (..)
import SideBar.Model as Model
import SideBar.Controls as Controls
import SideBar.Watches as Watches


view : Bool -> Int -> Model.Model -> Html
view showSwap height state =
  -- TODO: event blocker, errors
  sidebar showSwap height state


sidebar : Bool -> Int -> Model.Model -> Html
sidebar showSwap height state =
  let
    constantStyles =
      [ ("background", colorToCss darkGrey) -- dark grey
      , ("width", intToPx sidebarWidth)
      , ("height", "100%")
      , ("position", "absolute")
      , ("top", "0px")
      , ("right", "0px")
      , ("transition-duration", "0.3s")
      , ("opacity", "0.97")
      , ("z-index", "1")
      ]
    
    toggleStyles =
      if state.sidebarVisible
      then [ ("right", "0px")
           , ("width", intToPx sidebarWidth)
           ]
      else [("width", "0px")]
  in
    div
      [ id "elm-reactor-side-bar"
      -- TODO: done in JS: cancelBubble / stopPropagation on this
      , style (constantStyles ++ toggleStyles)
      ]
      [ sidebarTab state
      , div
          [ style [ ("height", "100%") ] ]
          [ Controls.view showSwap state
          , Watches.view (height - Controls.controlsHeight) state.watches
          ]
      ]


tabWidth = 25

sidebarTab : Model.Model -> Html
sidebarTab state =
  div
    [ style
        [ ("position", "absolute")
        , ("width", intToPx tabWidth)
        , ("height", "60px")
        , ("top", "50%")
        , ("left", intToPx -tabWidth)
        , ("border-top-left-radius", "3px")
        , ("border-bottom-left-radius", "3px")
        , ("background", colorToCss darkGrey)
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
  Signal.map2 (view showSwap) Window.height scene


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

permitSwapMailbox = Controls.permitSwapMailbox

restartMailbox = Controls.restartMailbox

pausedInputMailbox = Controls.pausedInputMailbox

scrubMailbox = Controls.scrubMailbox

buttonStateMailbox = Controls.buttonStateMailbox

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
