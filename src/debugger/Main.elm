module Main where

import Color
import Dict
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode as JsDec
import Json.Encode as JsEnc
import Task exposing (Task)

import Effects exposing (..)
import StartApp
import WebSocket
import Html.File as File

import Debugger.Active as Active
import Debugger.Model as DM
import Debugger.RuntimeApi as API
import Debugger.Service as Service
import Explorer.Logs as Logs
import Model exposing (..)
import SideBar.Button as Button
import SideBar.Controls as Controls
import SideBar.ActionLog as ActionLog
import SideBar.Footer as Footer
import Utils.Helpers exposing (unsafe, unsafeResult)
import Utils.Style exposing (colorToCss, darkGrey, lightGrey)



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
    , view = view { openFileChooser = openFileChooserMailbox.address }
    , update = update { autoscrollLog = autoscrollLogMailbox.address }
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


-- TODO: generate with (Stylesheets.prettyPrint 4 HomepageStylesheet.exports)
styleStr : String
styleStr = """
html, body, body > div, .container, .left-sidebar {
  height: 100%;
}

.container {
  display: flex;
  flex-direction: row;
}

.left-sidebar {
  display: flex;
  flex-direction: column;
  width: 350px;
}

""" ++ Controls.styles ++ ActionLog.styles ++ Footer.styles

type alias ViewPorts =
  { openFileChooser : Signal.Address String }

view : ViewPorts -> Signal.Address Message -> Model -> Html
view ports addr state =
  case state.serviceState of
    Just activeState ->
      div
        [ class "container" ]
        [ node "style" [ Attr.property "innerHTML" (JsEnc.string styleStr) ] []

          -- left sidebar
        , div
            [ class "left-sidebar" ]
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
            , Footer.view
                { changeFile = (\files -> Signal.message addr (ImportSession files))
                , clickExport = Signal.message addr ExportSession
                , clickImport = Signal.message ports.openFileChooser
                }
                (case state.swapSocket of
                    Just _ ->
                      "socket connected"

                    Nothing ->
                      "socket not connected"
                )
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
                    state.logsState
                    activeState
                ]
            ]
        ]

    Nothing ->
      -- TODO: prettify
      div
        []
        [ text "Initializing..." ]


type alias UpdatePorts =
  { autoscrollLog : Signal.Address String }

update : UpdatePorts -> Message -> Model -> (Model, Effects Message)
update ports msg state =
  case msg of
    SidebarVisible visible ->
      ( { state | sidebarVisible = visible }
      , none
      )

    PermitSwaps permit ->
      ( { state | permitSwaps = permit }
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
              | serviceState = serviceState
              , logsState = fst (Logs.update logMsg state.logsState)
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
        ( { state | playPauseButtonState = newState }
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
        ( { state | restartButtonState = newState }
        , sendEffects
        )

    LogsMessage logMsg ->
      let
        (newLogsState, maybeFrame) =
          Logs.update logMsg state.logsState

        sendEffect =
          maybeFrame
            |> Maybe.map
                (\frameIdx -> Signal.send
                  (Service.commandsMailbox ()).address
                  (Active.ScrubTo frameIdx)
                )
            |> Maybe.withDefault
                (Signal.send ports.autoscrollLog "#action-log")
            |> Task.map (\_ -> NoOp)
            |> task
      in
        ( { state | logsState = newLogsState }
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
      ( { state | swapSocket = maybeSocket }
      , none
      )

    SwapEvent swapEvt ->
      if state.permitSwaps then
        case swapEvt of
          NewModuleEvent compiledMod ->
            ( { state | errorState = NoErrors }
            , Signal.send
                (Service.commandsMailbox ()).address
                (Active.Swap compiledMod)
              |> Task.map (always NoOp)
              |> task
            )

          CompilationErrorsEvent errs ->
            ( { state | errorState = CompilationErrors errs }
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
          JsDec.decodeString DM.decodeSessionRecord str
            |> Result.formatError ParseError

        sessionRecordTask : Task x (Result SessionInputError DM.SessionRecord)
        sessionRecordTask =
          files
            |> List.head
            |> unsafe "files list empty"
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
      ( { state | errorState = SessionInputError error }, none )

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
        ( { state | errorState = newErrorState }, none )

    CloseErrors ->
      ( { state | errorState = NoErrors }, none )

    NoOp ->
     ( state, none )


port autoscrollLog : Signal String
port autoscrollLog =
  autoscrollLogMailbox.signal

autoscrollLogMailbox : Signal.Mailbox String
autoscrollLogMailbox =
  Signal.mailbox ""


port openFileChooser : Signal String
port openFileChooser =
  openFileChooserMailbox.signal

openFileChooserMailbox : Signal.Mailbox String
openFileChooserMailbox =
  Signal.mailbox ""


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
              |> unsafeResult
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

    getContents =
      API.getSessionRecord session
        |> Task.map (JsEnc.encode 4 << DM.encodeSessionRecord)

    downloadContents contents =
      File.download contents exportMimeType fileName
  in
    getContents `Task.andThen` downloadContents


