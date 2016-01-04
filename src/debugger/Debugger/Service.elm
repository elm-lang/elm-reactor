module Debugger.Service where

import Signal
import Task
import Debug
import Html exposing (div)

import Effects exposing (..)
import StartApp

import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Debugger.Active as Active


type alias Model =
  Maybe Active.Model


initModel : Model
initModel =
  Nothing


type Message
  = Initialized DM.DebugSession DM.ValueSet DM.Window
  | ActiveMessage Active.Message


app : DM.ModuleName -> StartApp.Config Model Message
app moduleName =
  let
    window_ =
      API.opener
  in
    { init =
        ( Nothing
        , (API.getFromGlobalScope window_ moduleName
            |> Task.mapError
                    (\err -> Debug.crash <| "module name not in scope: " ++ err))
          `Task.andThen` (\initModule ->
            API.start
              window_
              initModule
              (Signal.forwardTo notificationsMailbox.address Active.NewFrame)
            |> Task.map (\(session, valueSet) -> Initialized session valueSet window_)
          )
          |> task
        )
    , view = \_ _ -> div [] [] -- would be nice to not do this
    , update = update
    , inputs =
        [ Signal.map
            (ActiveMessage << Active.Notification)
            notificationsMailbox.signal
        , Signal.map
            (ActiveMessage << Active.Command)
            (commandsMailbox ()).signal
        , Signal.map
            (ActiveMessage << Active.UiMessage)
            (Active.valueExplorerActionMailbox ()).signal
        ]
    }


-- TODO: there's an error if I make this a value instead of a function. Why?
commandsMailbox : () -> Signal.Mailbox Active.Command
commandsMailbox _ =
  mailbox


mailbox =
  Signal.mailbox Active.NoOpCommand


notificationsMailbox : Signal.Mailbox Active.Notification
notificationsMailbox =
  Signal.mailbox Active.NoOpNot


update : Message -> Model -> (Model, Effects Message)
update msg state =
  case msg of
    Initialized session initValues window_ ->
      case state of
        Just _ ->
          Debug.crash "already initialized"

        Nothing ->
          ( Just (Active.initModel window_ initValues session)
          , none
          )

    ActiveMessage actMsg ->
      case state of
        Just activeModel ->
          let
            (newActiveModel, activeFx) =
              Active.update actMsg activeModel

          in
            ( Just newActiveModel
            , Effects.map ActiveMessage activeFx
            )

        Nothing ->
          Debug.crash "not yet initialized"
