module SideBar where

import Graphics.Element (..)
import Html
import List
import Signal
import Window

import Controls
import Model (..)
import Watches


view : (Int, Int) -> List (String, String) -> Bool -> State -> Element
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


scene : Signal State
scene =
  Signal.foldp step startState aggregateUpdates


aggregateUpdates : Signal Update
aggregateUpdates =
  Signal.mergeMany
    [ Signal.map (always Restart) (Signal.subscribe Controls.restartChannel)
    , Signal.map Pause (Signal.subscribe Controls.pausedInput)
    , Signal.map TotalEvents eventCounter
    , Signal.map ScrubPosition (Signal.subscribe Controls.scrupChannel)
    ]


-- INCOMING PORTS

port eventCounter : Signal Int

port watches : Signal (List (String, String))

port showSwap : Bool


-- OUTGOING PORTS

port scrubTo : Signal Int
port scrubTo =
    Signal.map .scrubPosition scene


port pause : Signal Bool
port pause =
    Signal.map .paused scene


port restart : Signal Int
port restart =
    Signal.map (always 0) (Signal.subscribe Controls.restartChannel)


port permitSwap : Signal Bool
port permitSwap =
    Signal.subscribe Controls.permitSwapChannel
