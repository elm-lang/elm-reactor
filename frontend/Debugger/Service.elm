module Debugger.Service where

import Signal
import Task
import Debug
import Html exposing (div)

import Transaction exposing (..)
import Start

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


app : API.ModuleName -> Start.Config Message Model
app moduleName =
  { init =
      let
        initTask =
          (API.getFromGlobalScope moduleName
            |> Task.mapError
                  (\err -> Debug.crash <| "module name not in scope: " ++ err))
          `Task.andThen` (\initModule ->
            API.initializeFullscreen
              initModule
              (Signal.forwardTo notificationsMailbox.address Active.NewFrame)
              API.justMain
            |> Task.map (\(session, values) -> Initialized session values)
          )
      in
        requestTask initTask Nothing
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


update : Message -> Model -> Transaction Message Model
update msg model =
  case Debug.log "SERVICE MSG" msg of
    Initialized session initValues ->
      case model of
        Just _ ->
          Debug.crash "already initialized"

        Nothing ->
          Active.initModel session
            |> Just
            |> done

    ActiveMessage actMsg ->
      case model of
        Just activeModel ->
          with
            (tag ActiveMessage <| Active.update actMsg activeModel)
            (done << Just)

        Nothing ->
          Debug.crash "not yet initialized"
