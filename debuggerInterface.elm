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
    }

type State =
    { paused : Bool
    , events : (Int, Int)
    }

paused : Input Bool
paused = input False

-- inport placeholder
port eventCounter : Signal Int
--eventCounter = count <| fps 2

-- View

objHeight = 40

playButton : Element
playButton =
    let icon = [ngon 3 15.0 |> filled red]
    in  collage 40 40 icon
            |> clickable paused.handle False

pauseButton : Element
pauseButton =
    let icon = [ rect 7.5 20
                    |> filled red
                    |> moveX -6
               , rect 7.5 20
                    |> filled red
                    |> moveX 6
                ]
    in collage 40 objHeight icon
            |> clickable paused.handle True

restartButton : Element
restartButton =
    let icon = [circle 15.0 |> filled orange]
    in  collage 40 objHeight icon
            |> clickable paused.handle False

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

port controls : Signal State
port controls = scene

scene : Signal State
scene = foldp step startState aggregateUpdates

step : Update -> State -> State
step update state =
    let currentEvent = case update.paused of
            True -> sliderPosition update
            False -> update.maxEvents
        _ = Debug.watch "state" state
    in  { paused = update.paused
        , events = (currentEvent, update.maxEvents)
        }

startState : State
startState =
    { paused = False
    , events = (0,0)
    }

aggregateUpdates : Signal Update
aggregateUpdates =
    let dampenedMouse = sampleOn Mouse.isDown Mouse.position
        dampnededEvents = keepWhen (lift (not) paused.signal) 0 eventCounter
        mouse down (x,y) =
            { down = down
            , position = (toFloat x, toFloat y)
            }
        combinedMouse = lift2 mouse Mouse.isDown dampenedMouse
        aggregator window mouse paused eventCounter =
            { mouse = mouse
            , window = window
            , maxEvents = eventCounter
            , paused = paused
            }
    in  lift4 aggregator Window.dimensions combinedMouse paused.signal dampnededEvents
