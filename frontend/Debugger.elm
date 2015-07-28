module Debugger where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Signal
import Task exposing (Task)
import Json.Decode as JsDec
import String

import FancyStartApp
import Empty exposing (..)

import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Debugger.Service as Service

type alias Model =
  { sidebarVisible : Bool
  , permitSwaps : Bool
  , serviceState : DM.Model
  }


initModel : Model
initModel =
  { sidebarVisible = True
  , permitSwaps = True
  , serviceState = DM.Uninitialized
  }


type Action
  = SidebarVisible Bool
  | PermitSwaps Bool
  | NewServiceState DM.Model
  | CompilationErrors CompilationErrors


type alias CompilationErrors =
  String


(html, uiTasks) =
  FancyStartApp.start
    { initialState = initModel
    , initialTasks = (\loopback ->
        [Signal.send (Service.commandsMailbox ()).address (DM.Initialize initMod)])
    , externalActions =
        Signal.map NewServiceState Service.state
    , view = view
    , update = update
    }

main =
  html


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr state =
  let
    mainVal =
      case state.serviceState of
        DM.Active activeAttrs ->
          activeAttrs.mainVal

        _ ->
          div [] []

  in
    div []
      [ div []
          [ mainVal ]
      , viewSidebar addr state
      ]


viewSidebar : Signal.Address Action -> Model -> Html
viewSidebar addr state =
  let
    body =
      case state.serviceState of
        DM.Active activeAttrs ->
          activeSidebarBody addr activeAttrs

        _ ->
          text "Initialzing..."
  in
    -- TODO: event blocker
    -- TODO: toggle tab
    div
      [ style
          [ "position" => "absolute"
          , "width" => "300px"
          , "right" => "0px"
          , "background-color" => "gray"
          , "color" => "white"
          , "z-index" => "1"
          ]
      ]
      [ body ]


activeSidebarBody : Signal.Address Action -> DM.ActiveAttrs -> Html
activeSidebarBody addr activeAttrs =
  let
    commandsAddr =
      (Service.commandsMailbox ()).address

    numFrames =
      DM.numFrames activeAttrs

    curFrame =
      DM.curFrameIdx activeAttrs
  in
    div []
      [ div []
          [ button
              [ onClick
                  commandsAddr
                  (if DM.isPlaying activeAttrs then
                    DM.Pause
                  else
                    DM.ForkFrom curFrame True)
              ]
              [ text
                  (if DM.isPlaying activeAttrs then "Pause" else "Play")
              ]
          , button
              [ onClick commandsAddr <|
                  DM.ForkFrom 0 (DM.isPlaying activeAttrs)
              ]
              [ text "Reset" ]
          ]
      , div []
          [ div []
          [ text <|
              "frame idx: "
              ++ (toString <| curFrame)
              ++ "; numFrames: " ++ toString numFrames
          , input
              [ type' "range"
              , Attr.min "0"
              , Attr.max <| toString <| numFrames - 1
              , Attr.value <| toString <| curFrame
              , on
                  "input"
                  (JsDec.at ["target","value"]
                    (JsDec.customDecoder JsDec.string String.toInt))
                  (\idx ->
                    Signal.message
                    commandsAddr
                    (DM.GetNodeState {start=idx, end=idx} [DM.mainId activeAttrs]))
              ]
              []
            ]
          ]
      ]


update : FancyStartApp.UpdateFun Model Empty Action
update loopback now action state =
  case action of
    SidebarVisible visible ->
      ( { state | sidebarVisible <- visible }
      , []
      )

    PermitSwaps permitSwaps ->
      ( { state | permitSwaps <- permitSwaps }
      , []
      )

    NewServiceState serviceState ->
      ( { state | serviceState <- serviceState }
      , []
      )

-- INPUT PORT: initial module

port initMod : API.ElmModule

-- TASK PORTS

port uiTasksPort : Signal (Task Empty ())
port uiTasksPort =
  uiTasks

port debugServiceTasks : Signal (Task Empty ())
port debugServiceTasks =
  Service.tasks
