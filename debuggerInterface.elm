module DebuggerInterface where

import Window
import Graphics.Input (..)
import Debug

-- Model

data ExecutionState
    = Paused Int
    | Running

executionState : Input ExecutionState
executionState = input Running

-- View

playButton : Element
playButton =
    let icon = [ngon 3 15.0 |> filled red]
    in  collage 40 40 icon
            |> clickable executionState.handle Running

pauseButton : Element
pauseButton =
    let icon = [ rect 7.5 20
                    |> filled red
                    |> moveX -6
               , rect 7.5 20
                    |> filled red
                    |> moveX 6
                ]
    in collage 40 40 icon
            |> clickable executionState.handle (Paused 0)

restartButton : Element
restartButton =
    let icon = [circle 15.0 |> filled orange]
    in  collage 40 40 icon
            |> clickable executionState.handle Running


view : ExecutionState -> Element
view state =
    (case state of
        Paused position -> playButton
        Running -> pauseButton)
    `beside` restartButton

main = lift2 scene executionState.signal Window.dimensions

scene : ExecutionState -> (Int,Int) -> Element
scene state (w,h) =
    let watchedState = Debug.watch "state" state
    in  view watchedState


