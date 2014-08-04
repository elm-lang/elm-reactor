module DebuggerInterface where

import Window
import Graphics.Input (..)
import Mouse
import Debug

-- Model

type Mouse =
    { down : Bool
    , position : (Float, Float)
    }

type Update =
    { mouse : Mouse
    , window : (Int, Int)
    , maxEvents : Int
    , paused : Bool
    , restart : Bool
    }

type State =
    { paused : Bool
    , events : (Int, Int)
    }

{-
data Action
    = Noop
    | Pause
    | Continue
    | Restart
    | Scrub Int
-}

pausedInput : Input Bool
pausedInput = input False

restartInput : Input ()
restartInput = input ()

-- inport placeholder
port eventCounter : Signal Int
--eventCounter = count <| fps 2

-- View

objHeight = 40
buttonWidth = 40

playButton : Element
playButton =
    let icon = [ngon 3 15.0 |> filled red]
    in  collage buttonWidth objHeight icon
            |> clickable pausedInput.handle False

pauseButton : Element
pauseButton =
    let icon = [ rect 7.5 20
                    |> filled red
                    |> moveX -6
               , rect 7.5 20
                    |> filled red
                    |> moveX 6
                ]
    in collage buttonWidth objHeight icon
            |> clickable pausedInput.handle True

restartButton : Element
restartButton =
    let icon = [circle 15.0 |> filled orange]
    in  collage buttonWidth objHeight icon
            |> clickable restartInput.handle ()

timelinePath : (Int, Int) -> Element
timelinePath (w,_) =
    let w' = toFloat w
        lengthOffset = 100
        emptyTimeline = rect (w' - lengthOffset) 2
            |> filled black
            |> moveX 15
    in  collage (w - lengthOffset) objHeight [emptyTimeline]

linearScale : (Float, Float) -> (Float, Float) -> Float -> Float
linearScale (domainMin, domainMax) (rangeMin, rangeMax) x =
    rangeMin + ((rangeMax - rangeMin) * (x - domainMin) / (domainMax - domainMin))

timelineFilled : (Int, Int) -> State -> Element
timelineFilled (w,_) state =
    let w' = toFloat w
        lengthOffset = 100
        collageOffset = 10
        len = w' - lengthOffset
        currentLocation = case state.paused of
            True ->
                let max = toFloat <| snd state.events
                    curr = toFloat <| fst state.events
                in  linearScale (0,max) (-len/2 + 17,len/2) curr
            False -> w' / 2 - 65 + collageOffset
        indicator = rect 10 25
            |> filled blue
            |> moveX currentLocation
    in  collage (w - lengthOffset) objHeight [indicator]

view : (Int, Int) -> State -> Element
view dim state =
    (case state.paused of
        True -> playButton
        False -> pauseButton)
    `beside` restartButton
    `beside`  layers [ timelinePath dim
                     , timelineFilled dim state
                     ]

-- Calcuations from position

mouseOnTimeline : Update -> Maybe Int
mouseOnTimeline update =
    let mx = fst update.mouse.position
        leftOffset = sum <| map widthOf [playButton, restartButton]
        mx' = mx - toFloat leftOffset
        timeLineWidth = toFloat <| widthOf <| timelinePath update.window
        scale m = linearScale (15, timeLineWidth) (0, toFloat update.maxEvents) m
    in  case mx' >= 15 && mx' <= timeLineWidth of
            True -> Just <| round <| scale mx'
            False -> Nothing

sliderPosition : Update -> Int
sliderPosition update =
    case mouseOnTimeline update of
        Just n -> n
        Nothing -> update.maxEvents

-- The wiring

main : Signal Element
main = lift2 view Window.dimensions scene

port scrub : Signal Int
port scrub = dropRepeats <| lift (fst . .events) scene

port pause : Signal Bool
port pause = pausedInput.signal

port restart : Signal Int
port restart = lift (\x -> 0) restartInput.signal

scene : Signal State
scene = foldp step startState aggregateUpdates

step : Update -> State -> State
step update state =
    let currentEvent = sliderPosition update
    in  case update.restart of
            True -> startState
            False ->
                { paused = update.paused
                , events = (currentEvent, update.maxEvents)
                }

startState : State
startState =
    { paused = False
    , events = (0,0)
    }

aggregateUpdates : Signal Update
aggregateUpdates =
    let dampenedMouse = keepWhen Mouse.isDown (0,0) Mouse.position
        dampenedEvents = keepWhen (lift (not) pausedInput.signal) 0 eventCounter
        restartBool = merge (sampleOn combinedMouse <| constant False) (lift (\x -> True) restartInput.signal)
        mouse down (x,y) =
            { down = down
            , position = (toFloat x, toFloat y)
            }
        combinedMouse = lift2 mouse Mouse.isDown dampenedMouse
        aggregator window mouse paused restart eventCounter =
            { mouse = mouse
            , window = window
            , maxEvents = eventCounter
            , paused = paused
            , restart = restart
            }
    in  lift5 aggregator Window.dimensions combinedMouse pausedInput.signal restartBool dampenedEvents
