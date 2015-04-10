module SideBar where

import Graphics.Element exposing (..)
import Html
import List
import Window

import SideBar.Controls as Controls
import SideBar.Model as Model
import SideBar.Watches as Watches


view : (Int, Int) -> List (String, String) -> Bool -> Model.Model -> Element
view (w,h) watches permitSwap state =
  let controls =
          Controls.view (w, h) showSwap permitSwap state

      watchView =
          Html.toElement w (h - 150) (Watches.view watches)
  in
      flow down
        [ controls
        , watchView
        ]


-- SIGNALS    

main : Signal Element
main =
  Signal.map4 view
    (Signal.map (\(w,h) -> (Controls.panelWidth, h)) Window.dimensions)
    watches
    (Signal.subscribe Controls.permitSwapChannel)
    scene


scene : Signal Model.Model
scene =
  Signal.foldp Model.update Model.startModel aggregateUpdates


aggregateUpdates : Signal Model.Action
aggregateUpdates =
  Signal.mergeMany
    [ Signal.map (always Model.Restart) (Signal.subscribe Controls.restartChannel)
    , Signal.map Model.Pause (Signal.subscribe Controls.pausedInput)
    , Signal.map Model.TotalEvents eventCounter
    , Signal.map Model.ScrubPosition (Signal.subscribe Controls.scrubChannel)
    ]


-- INCOMING PORTS

port eventCounter : Signal Int

port watches : Signal (List (String, String))

port showSwap : Bool


-- OUTGOING PORTS

port scrubTo : Signal Int
port scrubTo =
    Signal.subscribe Controls.scrubChannel


port pause : Signal Bool
port pause =
    Signal.subscribe Controls.pausedInput


port restart : Signal Int
port restart =
    Signal.map (always 0) (Signal.subscribe Controls.restartChannel)


port permitSwap : Signal Bool
port permitSwap =
    Signal.subscribe Controls.permitSwapChannel
