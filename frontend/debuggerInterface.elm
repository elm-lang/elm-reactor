module DebuggerInterface where

import Graphics.Element (..)
import List
import Signal
import Signal (Signal, (<~), (~))
import Window

import Watches
import Controls
import Model (..)


view : (Int, Int) -> List (String, String) -> Bool -> State -> Element
view (w,h) watches permitSwap state =
    let controls = Controls.view (w, h) showSwap permitSwap state
        watchView = Watches.view (w, (h - 150)) watches
    in  flow down
            [ controls
            , watchView
            ]
        
--
-- The wiring
--

main : Signal Element
main = view <~ ((\(w, h) -> (Controls.panelWidth, h)) <~ Window.dimensions)
             ~ watches
             ~ (Signal.subscribe Controls.permitSwapChannel)
             ~ scene

port scrubTo : Signal Int
port scrubTo = .scrubPosition <~ scene

port pause : Signal Bool
port pause = .paused <~ scene

port restart : Signal Int
port restart =
    Signal.map (always 0) (Signal.subscribe Controls.restartChannel)

port permitSwap : Signal Bool
port permitSwap =
    Signal.subscribe Controls.permitSwapChannel


port eventCounter : Signal Int

port watches : Signal (List (String, String))

port showSwap : Bool

scene : Signal State
scene =
  Signal.foldp step startState aggregateUpdates

aggregateUpdates : Signal Update
aggregateUpdates =
  Signal.mergeMany
    [ always Restart <~ Signal.subscribe Controls.restartChannel
    , Pause <~ Signal.subscribe Controls.pausedInput
    , TotalEvents <~ eventCounter
    , ScrubPosition <~ Signal.subscribe Controls.scrupChannel
    ]

