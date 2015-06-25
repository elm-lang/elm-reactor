module SideBar where

import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Window

import SideBar.Controls as Controls
import SideBar.Model as Model
import SideBar.Watches as Watches


view : List (String, String) -> Bool -> Model.Model -> Html
view watches permitSwap state =
  let
    controls =
      Controls.view showSwap permitSwap state

    watchView =
      Watches.view watches
  in
    div
      []
      [ controls
      , watchView
      ]


-- SIGNALS

main : Signal Html
main =
  Signal.map3 view
    watches
    permitSwapMailbox.signal
    scene


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
