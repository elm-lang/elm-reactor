module Debugger.Service where

import Signal
import Task
import Empty exposing (Empty)
import Components exposing (..)
import Debug

import Debugger.RuntimeApi as API
import Debugger.Active as Active

type alias Model =
  Maybe Active.Model


type Message
  = Initialized API.DebugSession API.ValueSet
  | ActiveMessage Active.Message


app : API.ElmModule -> App Message Model Model
app initMod =
  { init =
      let
        effect =
          API.initializeFullscreen
            initMod
            API.emptyInputHistory
            (Signal.forwardTo (Active.notificationsMailbox ()).address Active.NewFrame)
            API.justMain
          |> Task.map (\(session, values) -> Initialized session values)
          |> task
      in
        -- would be nice to pipeline this
        request effect Nothing
  , view = always identity
  , update = update
  , externalMessages = Nothing
  }


update : Message -> Model -> Transaction Message Model
update msg model =
  case msg of
    Initialized session initValues ->
      case model of
        Just _ ->
          Debug.crash "already initialized"

        Nothing ->
          let
            initMain =
              Active.getMainVal session initValues
          in
            Active.initModel session initMain
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
