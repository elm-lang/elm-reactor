module Debugger where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Signal
import Task exposing (Task)
import Json.Decode as JsDec
import Color
import Maybe
import Result
import Debug

import Transaction exposing (..)
import Start
import WebSocket
import Html.File as File

import Model exposing (..)
import Styles exposing (..)
import Button
import Debugger.RuntimeApi as API
import Debugger.Active as Active
import Debugger.Service as Service
import SideBar.Controls as Controls
import SideBar.Logs as Logs
import DataUtils exposing (..)


serviceOutput : Start.App Service.Model
serviceOutput =
  Start.start <| Service.app initMod


output : Start.App Model.Model
output =
  Start.start
    { init =
        request (task connectSocket) initModel
    , view = view
    , update = update
    , inputs =
        [ Signal.map NewServiceState serviceOutput.model
        , socketEventsMailbox.signal
        ]
    }


main : Signal Html
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
        Just activeState ->
          (activeState.mainVal, Active.isPlaying activeState)

        Nothing ->
          (div [] [], False)

    errorOverlay =
      state.compilationErrors
        |> Maybe.map (\errs -> viewErrors errs)
        |> maybeToList

  in
    div []
      ( [ mainVal
        , eventBlocker (not isPlaying)
        ]
        ++ errorOverlay
        ++ [ viewSidebar addr state ]
      )


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


viewErrors : CompilationErrors -> Html
viewErrors errors =
  pre
    [ style
        [ "z-index" => "1"
        , "position" => "absolute"
        , "top" => "0"
        , "left" => "0"
        , "color" => colorToCss darkGrey
        , "background-color" => colorToCss lightGrey
        , "padding" => "1em"
        , "margin" => "1em"
        , "border-radius" => "10px"
        ]
    ]
    [ text errors ]


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
        Just activeState ->
          [ case state.swapSocket of
              Just _ ->
                text "socket connected"

              Nothing ->
                text "socket not connected"

          , Controls.view
              addr
              state
              activeState
          , Logs.view
              (Signal.forwardTo addr LogsMessage)
              Controls.totalHeight
              state.logsState
              activeState
          , exportImport addr
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
      -- TODO in JS: cancelBubble / stopPropagation on this
      , style (constantStyles ++ toggleStyles)
      ]
      ([toggleTab addr state] ++ body)


exportImport : Signal.Address Message -> Html
exportImport addr =
  div
    [ style
        [ "bottom" => "0"
        , "position" => "absolute"
        , "border-top" => "1px solid lightgrey"
        , "width" => "100%"
        , "display" => "flex"
        , "justify-content" => "space-around"
        , "color" => colorToCss lightGrey
        , "text-decoration" => "underline"
        ]
    ]
    [ span
        [ style ["cursor" => "pointer"]
        , onClick addr ExportSession
        ]
        [ text "export session" ]
    , input
        [ type' "file"
        , accept "application/json"
        , on
            "change"
            (JsDec.at ["target", "files"] <| File.domList File.file)
            (\files -> Signal.message addr (ImportSession files))
        ]
        []
    ]


update : Message -> Model -> Transaction Message Model
update msg state =
  case Debug.log "MAIN MSG" msg of
    SidebarVisible visible -> 
      done { state | sidebarVisible <- visible }

    PermitSwaps permit -> 
      done { state | permitSwaps <- permit }

    NewServiceState serviceState -> 
      let
        logMsg =
          case serviceState of
            Just active ->
              Logs.UpdateLogs
                { newNodeLogs = active.nodeLogs
                , newExprLogs = active.exprLogs
                }

            Nothing ->
              Logs.NoOp
      in
        with
          (tag LogsMessage <| Logs.update logMsg state.logsState)
          (\(newLogsState, _) ->
            done
              { state
                  | serviceState <- serviceState
                  , logsState <- newLogsState
              }
          )

    PlayPauseButtonAction buttonMsg ->
      with
        (tag PlayPauseButtonAction <|
          Button.update buttonMsg state.playPauseButtonState)
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
              request
                (task sendEffect)
                { state | playPauseButtonState <- newState }
        )

    RestartButtonAction buttonMsg -> 
      with
        (tag RestartButtonAction <|
          Button.update buttonMsg state.restartButtonState)
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
              request
                (task sendEffect)
                { state | restartButtonState <- newState }
        )

    LogsMessage logMsg -> 
      with
        (tag LogsMessage <| Logs.update logMsg state.logsState)
        (\(newLogsState, maybeFrame) ->
          let
            sendScrubTo =
              case maybeFrame of
                Just frameIdx ->
                  Signal.send
                    (Service.commandsMailbox ()).address
                    (Active.ScrubTo frameIdx)
                  |> Task.map (always NoOp)

                Nothing ->
                  Task.succeed NoOp
          in
            requestTask sendScrubTo { state | logsState <- newLogsState }
        )

    ConnectSocket maybeSocket ->
      done { state | swapSocket <- maybeSocket }

    SwapEvent swapEvt ->
      if state.permitSwaps then
        case swapEvt of
          NewModule compiledMod ->
            requestTask
              (Signal.send
                (Service.commandsMailbox ()).address
                (Active.Swap compiledMod)
              |> Task.map (always NoOp))
              { state | compilationErrors <- Nothing }

          CompilationErrors errs ->
            done { state | compilationErrors <- Just errs }
      else
        done state

    ServiceCommand serviceCmd -> 
      requestTask
        (Signal.send (Service.commandsMailbox ()).address serviceCmd
          |> Task.map (always NoOp))
        state

    ExportSession ->
      case state.serviceState of
        Just active ->
          requestTask
            (exportHistory active.session |> Task.map (always NoOp))
            state

        Nothing ->
          Debug.crash "can't export before initialized"

    ImportSession files ->
      let
        parseHistory : String -> Result SessionInputError API.InputHistory
        parseHistory str =
          API.parseInputHistory str
            |> Result.formatError ParseError

        contentsTask : Task x (Result SessionInputError API.InputHistory)
        contentsTask =
          files
            |> List.head
            |> getMaybe "files list empty"
            |> File.readAsText
            |> Task.mapError IoError
            |> Task.toResult
            |> Task.map (\res -> res `Result.andThen` parseHistory)

        sendTask : Task x Message
        sendTask =
          contentsTask
          `Task.andThen` (\historyRes ->
            case historyRes of
              Ok history ->
                Signal.send
                  (Service.commandsMailbox ()).address
                  (Active.StartWithHistory history)
                |> Task.map (always NoOp)

              Err err ->
                -- todo: send error message
                Debug.crash <| toString err
            )

      in
        requestTask sendTask state

    NoOp -> 
     done state


type SessionInputError
  = IoError File.IoError
  | ParseError API.InputHistoryParseError


-- Socket stuff

socketEventsMailbox : Signal.Mailbox Message
socketEventsMailbox =
  Signal.mailbox NoOp

-- INPUT PORT: initial module

port initMod : API.ElmModule

port fileName : String

-- would be nice to get this from a library
port windowLocationHost : String


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


exportMimeType =
  "application/json"


exportHistory : API.DebugSession -> Task x ()
exportHistory session =
  (API.getInputHistory session |> Task.map API.serializeInputHistory)
  `Task.andThen` (\contents ->
    File.download contents exportMimeType "reactor-history.json")
