module Debugger where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Signal
import Task exposing (Task)
import Json.Decode as JsDec
import String
import Color
import Debug

import Components exposing (..)
import Empty exposing (..)
import WebSocket

import Model exposing (..)
import Styles exposing (..)
import Button
import Debugger.RuntimeApi as API
import Debugger.Active as Active
import Debugger.Service as Service
import SideBar.Controls as Controls
import SideBar.Logs as Logs
import DataUtils exposing (..)


serviceOutput : Output Service.Model
serviceOutput =
  Service.app initMod
    |> Components.start


output : Output Model.Model
output =
  Components.start
    { init =
        request (task connectSocket) initModel
    , view = view
    , update = update
    , externalMessages =
        [ Signal.map NewServiceState serviceOutput.model
        , socketEventsMailbox.signal
        ]
    }


main =
  output.html


port uiTasks : Signal (Task Never ())
port uiTasks =
  output.tasks


port serviceTasks : Signal (Task Never ())
port serviceTasks =
  serviceOutput.tasks


(=>) = (,)


view : Signal.Address Message -> Model -> Html
view addr state =
  let
    (mainVal, isPlaying) =
      case state.serviceState of
        Just activeAttrs ->
          (activeAttrs.mainVal, Active.isPlaying activeAttrs)

        Nothing ->
          (div [] [], False)

  in
    div []
      [ mainVal
      , eventBlocker (not isPlaying)
      , viewSidebar addr state
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


tabWidth = 25

toggleTab : Signal.Address Message -> Model -> Html
toggleTab addr state =
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
    , onClick addr (SidebarVisible <| not state.sidebarVisible)
    ]
    []


viewSidebar : Signal.Address Message -> Model -> Html
viewSidebar addr state =
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
      if state.sidebarVisible then
        [ "right" => "0" ]
      else
        [ "right" => intToPx -sidebarWidth ]

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

    body =
      case state.serviceState of
        Just activeAttrs ->
          [ Controls.view
              addr
              state
              activeAttrs
          , dividerBar
          , Logs.view
              (Signal.forwardTo addr LogsAction)
              Controls.totalHeight
              state.logsState
              activeAttrs
          ]

        Nothing ->
          -- TODO: prettify
          [ div
              [style [ "color" => "white" ]]
              [text "Initialzing..."]
          ]
  in
    div
      [ id "elm-reactor-side-bar"
      -- done in JS: cancelBubble / stopPropagation on this
      , style (constantStyles ++ toggleStyles)
      ]
      ([toggleTab addr state] ++ body)


update : Message -> Model -> Transaction Message Model
update msg state =
  case Debug.log "MAIN MSG" msg of
    SidebarVisible visible -> 
      Debug.crash "SidebarVisible not implemented yet"

    PermitSwaps permit -> 
      Debug.crash "PermitSwaps not implemented yet"

    NewServiceState serviceState -> 
      done { state | serviceState <- serviceState }

    PlayPauseButtonAction buttonMsg ->
      with
        (tag PlayPauseButtonAction <| Button.update buttonMsg state.playPauseButtonState)
        (\(newState, maybeCommand) ->
          let
            sendEffect =
              case Debug.log "maybeCommand" maybeCommand of
                Just cmd ->
                  Signal.send (Service.commandsMailbox ()).address cmd
                    |> Task.map (always NoOp)

                Nothing ->
                  Task.succeed NoOp
            in
              request (task sendEffect) { state | playPauseButtonState <- newState }
        )

    RestartButtonAction buttonMsg -> 
      with
        (tag RestartButtonAction <| Button.update buttonMsg state.restartButtonState)
        (\(newState, maybeCommand) ->
          let
            sendEffect =
              case maybeCommand of
                Just cmd ->
                  Signal.send (Service.commandsMailbox ()).address cmd
                    |> Task.map (always NoOp)

                Nothing ->
                  Task.succeed NoOp
            in
              request (task sendEffect) { state | restartButtonState <- newState }
        )

    LogsAction logMsg -> 
      Debug.crash "LogsAction not implemented yet"

    ConnectSocket maybeSocket -> 
      done { state | swapSocket <- maybeSocket }

    SwapEvent swapEvt -> 
      Debug.crash "SwapEvent not implemented yet"

    ServiceCommand serviceCmd -> 
      request
        (Signal.send (Service.commandsMailbox ()).address serviceCmd
          |> Task.map (always NoOp)
          |> task)
        state

    NoOp -> 
     done state


-- Socket stuff

socketEventsMailbox : Signal.Mailbox Message
socketEventsMailbox =
  Signal.mailbox NoOp

-- INPUT PORT: initial module

port initMod : API.ElmModule

port fileName : String

-- would be nice to get this from a library
port windowLocationHost : String

-- TASK PORTS

connectSocket : Task Never Message
connectSocket =
  (WebSocket.create
    ("ws://" ++ windowLocationHost ++ "/socket?file=" ++ fileName)
    (Signal.forwardTo
      socketEventsMailbox.address
      (\evt ->
        case evt of
          WebSocket.Message msg ->
            msg
              |> JsDec.decodeString swapEvent
              |> getResult
              |> SwapEvent

          WebSocket.Close ->
            ConnectSocket Nothing
      )
    )
  )
  |> Task.map (ConnectSocket << Just)
