module Debugger.Service where

import Signal
import Task
import Debug
import Html exposing (div)

import Effects exposing (..)
import StartApp

import Debugger.RuntimeApi as API
import Debugger.Active as Active


type alias Model =
  Maybe Active.Model


initModel : Model
initModel =
  Nothing


type Message
  = Initialized API.DebugSession API.ValueSet
  | ActiveMessage Active.Message


--app : API.ModuleName -> StartApp.Config Message Model
app moduleName =
  { init =
      ( Nothing
      , (API.getFromGlobalScope moduleName
            |> Task.mapError
                  (\err -> Debug.crash <| "module name not in scope: " ++ err))
          `Task.andThen` (\initModule ->
            API.initializeFullscreenAndSubscribe
              initModule
              (Signal.forwardTo notificationsMailbox.address Active.NewFrame)
              API.justMain
            |> Task.map (\(session, values) -> Initialized session values)
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
update msg model =
  case Debug.log "SERVICE MSG" msg of
    Initialized session initValues ->
      case model of
        Just _ ->
          Debug.crash "already initialized"

        Nothing ->
          ( Just (Active.initModel session)
          , none
          )

    ActiveMessage actMsg ->
      case model of
        Just activeModel ->
          let
            (newActiveModel, activeFx) =
              Active.update actMsg activeModel

          in
            -- TODO: tag
            ( Just newActiveModel
            , Effects.map ActiveMessage activeFx
            )

        Nothing ->
          Debug.crash "not yet initialized"
