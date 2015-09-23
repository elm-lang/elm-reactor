module Debugger where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Signal
import Task exposing (Task)
import Json.Decode as JsDec
import Json.Encode as JsEnc
import Color
import Maybe
import Result
import Dict
import Debug

import Effects exposing (..)
import StartApp
import WebSocket
import Html.File as File

import Model exposing (..)
import Styles exposing (..)
import Button
import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Debugger.Active as Active
import Debugger.Service as Service
import SessionRecord as SessionRecord
import Controls
import Logs
import ActionLog
import DataUtils exposing (..)


serviceApp : StartApp.App Service.Model
serviceApp =
  StartApp.start <| Service.app moduleName


uiApp : StartApp.App Model.Model
uiApp =
  StartApp.start
    { init =
        ( initModel
        , connectSocket |> task
        )
    , view = view
    , update = update
    , inputs =
        [ Signal.map NewServiceState serviceApp.model
        , socketEventsMailbox.signal
        , (Active.commandResponseMailbox ()).signal
            |> Signal.map CommandResponse
        ]
    }


main : Signal Html
main =
  uiApp.html


port uiTasks : Signal (Task Never ())
port uiTasks =
  uiApp.tasks


port serviceTasks : Signal (Task Never ())
port serviceTasks =
  serviceApp.tasks


(=>) = (,)


viewErrors : Signal.Address Message -> ErrorState -> Html
viewErrors addr errors =
  let
    monospaceSpan content =
      span
        [ style ["font-family" => "monospace"] ]
        [ text content ]

    (visible, errorBody) =
      case errors of
        SwapReplayError {newShape, oldShape} ->
          ( True
          , div []
              [ h2 [] [ text "Events could not be replayed because the new signal graph has a different shape than the old one" ]
              , p [] [ text "Old signal graph:" ]
              , p [] [ monospaceSpan <| toString oldShape ]
              , p [] [ text "New signal graph:" ]
              , p [] [ monospaceSpan <| toString newShape ]
              ]
          )

        CompilationErrors errs ->
          (True, pre [] [ text errs ])

        HistoryMismatchError moduleNames ->
          ( True
          , div []
              [ h2 [] [ text "Import Session unsuccessful" ]
              , p []
                  [ text "You imported a session recorded on module "
                  , monospaceSpan moduleNames.historyModuleName
                  , text " but are currently running module "
                  , monospaceSpan moduleNames.currentModuleName
                  , text ". To fix, run the module the session was recorded on and try again."
                  ]
              ]
          )

        SessionInputError inputError ->
          let
            (errorType, errorMessage) =
              case inputError of
                IoError ioError ->
                  ("File I/O Error", ioError)

                ParseError parseError ->
                  ("JSON parse error", parseError)
          in
            ( True
            , div []
                [ h2 [] [ text "Import Session unsuccessful" ]
                , p []
                    [ text <| errorType ++ ": "
                    , monospaceSpan errorMessage
                    ]
                ]
            )

        NoErrors ->
          (False, text "")
  in
    div
      [ style
          [ "z-index" => "1"
          , "position" => "absolute"
          , "top" => "0"
          , "left" => "0"
          , "right" => "300px"
          , "color" => colorToCss darkGrey
          , "background-color" => colorToCss lightGrey
          , "padding" => "1em"
          , "margin" => "1em"
          , "border-radius" => "10px"
          , "display" => if visible then "block" else "none"
          ]
      ]
      [ errorBody
      , button
          [ onClick addr CloseErrors ]
          [ text "Close" ]
      ]


view : Signal.Address Message -> Model -> Html
view addr state =
  case state.serviceState of
    Just activeState ->
      div
        [ style
            [ "display" => "flex" 
            , "flex-direction" => "row"
            ]
        ]
        [ -- left sidebar
          div
            [ style
                [ "display" => "flex"
                , "flex-direction" => "column"
                ]
            ]
            [ div
                []
                [ Controls.view
                    addr
                    state
                    activeState
                , ActionLog.view
                    (Signal.forwardTo addr ActionLogMessage)
                    (case activeState.nodeLogs |> Dict.toList of
                      [(foldpParentId, actionLog)] ->
                        actionLog

                      -- TODO: it's always this on the first frame, but shouldn't be
                      -- need to refactor initialization process somehow
                      [] ->
                        []
                        --Debug.crash "no foldps"

                      -- TODO: handle these cases gracefully
                      _ ->
                        Debug.crash "multiple foldps"
                    )
                    (Active.curFrameIdx activeState)
                , div
                    [ style
                        [ ("background-color" => "darkgrey") ]
                    ]
                    [ case state.swapSocket of
                        Just _ ->
                          text "socket connected"

                        Nothing ->
                          text "socket not connected"
                    , exportImport addr
                    ]
                ]
            ]
          -- main area
        , div
            [ style
                [ "display" => "flex"
                , "flex-direction" => "row"
                ]
            ]
            [ div
                []
                [ Logs.view
                    (Signal.forwardTo addr LogsMessage)
                    Controls.totalHeight
                    state.logsState
                    activeState
                ]
            ]
        ]

    Nothing ->
      -- TODO: prettify
      div
        []
        [ text "Initialzing..." ]


exportImport : Signal.Address Message -> Html
exportImport addr =
  div
    [ style
        [ "display" => "flex"
        , "justify-content" => "space-around"
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


update : Message -> Model -> (Model, Effects Message)
update msg state =
  case msg of
    SidebarVisible visible -> 
      ( { state | sidebarVisible <- visible }
      , none
      )

    PermitSwaps permit -> 
      ( { state | permitSwaps <- permit }
      , none
      )

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
        ( { state
              | serviceState <- serviceState
              , logsState <- fst (Logs.update logMsg state.logsState)
          }
        , Effects.none
        )

    PlayPauseButtonAction buttonMsg ->
      let
        (newState, maybeCommand) =
          Button.update buttonMsg state.playPauseButtonState

        sendEffects =
          case maybeCommand of
            Just cmd ->
              Signal.send (Service.commandsMailbox ()).address cmd
                |> Task.map (always NoOp)
                |> task

            Nothing ->
              none
      in
        ( { state | playPauseButtonState <- newState }
        , sendEffects
        )

    RestartButtonAction buttonMsg ->
      let
        (newState, maybeCommand) =
          Button.update buttonMsg state.restartButtonState

        sendEffects =
          case maybeCommand of
            Just cmd ->
              Signal.send (Service.commandsMailbox ()).address cmd
                |> Task.map (always NoOp)
                |> task

            Nothing ->
              none
      in
        ( { state | restartButtonState <- newState }
        , sendEffects
        )

    LogsMessage logMsg ->
      let
        (newLogsState, maybeFrame) =
          Logs.update logMsg state.logsState

        sendEffect =
          case maybeFrame of
            Just frameIdx ->
              Signal.send
                (Service.commandsMailbox ()).address
                (Active.ScrubTo frameIdx)
              |> Task.map (always NoOp)
              |> task

            Nothing ->
              none
      in
        ( { state | logsState <- newLogsState }
        , sendEffect
        )

    ActionLogMessage (ActionLog.GoToFrame frameIdx) ->
      let
        sendScrubEffect =
          Signal.send
            (Service.commandsMailbox ()).address
            (Active.ScrubTo frameIdx)
          |> Task.map (always NoOp)
          |> task
      in
        ( state
        , sendScrubEffect
        )

    ConnectSocket maybeSocket ->
      ( { state | swapSocket <- maybeSocket }
      , none
      )

    SwapEvent swapEvt ->
      if state.permitSwaps then
        case swapEvt of
          NewModuleEvent compiledMod ->
            ( { state | errorState <- NoErrors }
            , Signal.send
                (Service.commandsMailbox ()).address
                (Active.Swap compiledMod)
              |> Task.map (always NoOp)
              |> task
            )

          CompilationErrorsEvent errs ->
            ( { state | errorState <- CompilationErrors errs }
            , none
            )
      else
        ( state, none )

    ServiceCommand serviceCmd -> 
      ( state
      , Signal.send (Service.commandsMailbox ()).address serviceCmd
          |> Task.map (always NoOp)
          |> task
      )

    ExportSession ->
      case state.serviceState of
        Just active ->
          ( state
          , exportHistory active.session
              |> Task.map (always NoOp)
              |> task
          )

        Nothing ->
          Debug.crash "can't export before initialized"

    ImportSession files ->
      let
        parseHistory : String -> Result SessionInputError DM.SessionRecord
        parseHistory str =
          JsDec.decodeString SessionRecord.decodeSessionRecord str
            |> Result.formatError ParseError

        sessionRecordTask : Task x (Result SessionInputError DM.SessionRecord)
        sessionRecordTask =
          files
            |> List.head
            |> getMaybe "files list empty"
            |> File.readAsText
            |> Task.mapError IoError
            |> Task.toResult
            |> Task.map (\res -> res `Result.andThen` parseHistory)

        sendTask : Task x Message
        sendTask =
          sessionRecordTask
          `Task.andThen` (\record ->
            case record of
              Ok history ->
                Signal.send
                  (Service.commandsMailbox ()).address
                  (Active.StartWithHistory history)
                |> Task.map (always NoOp)

              Err err ->
                -- maybe this should use a different action, since
                -- CommandResponses usually come from the Active module
                Task.succeed <| SessionInputErrorMessage err
          )

      in
        ( state, sendTask |> task )

    SessionInputErrorMessage error ->
      ( { state | errorState <- SessionInputError error }, none )

    CommandResponse responseMsg ->
      let
        newErrorState =
          case responseMsg of
            Active.SwapReplayError replayError ->
              SwapReplayError replayError

            Active.SwapSuccessful ->
              NoErrors

            Active.HistoryMismatchError moduleNames ->
              HistoryMismatchError moduleNames

            Active.ImportSessionSuccessful ->
              NoErrors

            Active.NoOpResponse ->
              state.errorState
      in
        ( { state | errorState <- newErrorState }, none )

    CloseErrors ->
      ( { state | errorState <- NoErrors }, none )

    NoOp -> 
     ( state, none )


-- Socket stuff

socketEventsMailbox : Signal.Mailbox Message
socketEventsMailbox =
  Signal.mailbox NoOp

-- INPUT PORT: initial module

port moduleName : DM.ModuleName

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


exportHistory : DM.DebugSession -> Task x ()
exportHistory session =
  let
    fileName =
      "reactor-history-" ++ moduleName ++ ".json"
  in
    (API.getSessionRecord session
      |> Task.map (\record ->
          SessionRecord.encodeSessionRecord record
            |> JsEnc.encode 4))
    `Task.andThen` (\contents ->
      File.download contents exportMimeType fileName)
