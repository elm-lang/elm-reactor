module Debugger.Service where

import Signal exposing (Signal)
import Task exposing (Task)
import Dict
import Set
import Html
import Debug

import FancyStartApp
import Empty exposing (Empty)

import Debugger.RuntimeApi as API
import Debugger.Model exposing (..)
import Debugger.Reflect as Reflect


state : Signal Model
state =
  fst stateAndTasks


tasks : Signal (Task Empty ())
tasks =
  snd stateAndTasks


stateAndTasks =
  FancyStartApp.start
    { initialState = Uninitialized
    -- w/b swapping?
    -- initialization happens later (?)
    , initialTasks = always []
    , externalActions =
        Signal.mergeMany
          [ Signal.map Notification notificationsMailbox.signal
          , Signal.map Command (commandsMailbox ()).signal
          ]
    , view = always identity
    , update = update
    }


-- why do I have to do this?
commandsMailbox : () -> Signal.Mailbox Command
commandsMailbox _ =
    mailbox


mailbox =
  Signal.mailbox NoOpCommand


notificationsMailbox : Signal.Mailbox Notification
notificationsMailbox =
  Signal.mailbox NoOpNot


{-| so what happens if you subscribe to something
while playing? Pause => 
-}

update : FancyStartApp.UpdateFun Model Empty Action
update loopback now action state =
  case state of
    Uninitialized ->
      case action of
        Command (Initialize mod) ->
          ( Initializing
          , [ API.initialize
                mod
                API.emptyInputHistory
                (Signal.forwardTo notificationsMailbox.address NewFrame)
                (\shape -> [shape.mainId])
              |> Task.map (\(session, values) -> Response <| IsActive session values)
              |> Task.mapError (\swapErr -> Debug.crash swapErr)
              |> loopback
            ]
          )

        _ ->
          Debug.crash "..."

    Initializing ->
      case action of
        Response (IsActive session values) ->
          ( Active <| initialActiveAttrs session (getMainVal session values)
          , []
          )

        _ ->
          Debug.crash "..."

    Active activeAttrs ->
      let
        (newAAs, tasks) = updateActive loopback now action activeAttrs
      in
        (Active newAAs, tasks)

updateActive : FancyStartApp.UpdateFun ActiveAttrs Empty Action
updateActive loopback now action state =
  let d = Debug.log "(act, state)" (action, state.sessionState)
  in case state.sessionState of
    Playing maybeCommand ->
      case maybeCommand of
        Just cmdOut ->
          case cmdOut of
            Swapping ->
              case action of
                Response (SwapResult res) ->
                  case res of
                    Ok values ->
                      ( { state | mainVal <- getMainVal state.session values
                                , sessionState <- Playing Nothing
                        }
                      , []
                      )

                    Err swapErr ->
                      ( { state | sessionState <- SwapError swapErr }
                      , []
                      )

                _ ->
                  Debug.crash "..."

            Subscribing bool ->
              case action of
                -- TODO: factor out SUB
                Response (IsSubscribed maybeVals) ->
                  (state, [])

                _ ->
                  Debug.crash "..."

            _ ->
              Debug.crash "..."

        Nothing ->
          case action of
            Notification not ->
              case not of
                NewFrame newFrame ->
                  let
                    curFrame =
                      curFrameIdx state

                    newExprLogs =
                      updateLogs
                        curFrame
                        state.exprLogs
                        newFrame.flaggedExprValues
                        (always True)

                    newNodeLogs =
                      updateLogs
                        curFrame
                        state.nodeLogs
                        newFrame.subscribedNodeValues
                        (\id -> id /= mainId state)

                    mainValue =
                      newFrame.subscribedNodeValues
                        |> List.filter (\(id, val) -> id == mainId state)
                        |> List.head
                        |> Maybe.map (snd >> Reflect.getHtml)
                        |> Maybe.withDefault state.mainVal
                  in
                    ( { state
                          | exprLogs <- newExprLogs
                          , nodeLogs <- newNodeLogs
                          , mainVal <- mainValue
                      }
                    , []
                    )

                NoOpNot ->
                  (state, [])

            Command Pause ->
              ( { state | sessionState <- Pausing }
              , [ API.setPlaying state.session False
                    |> Task.mapError (\_ -> Debug.crash "sup")
                    |> Task.map (always (Response <| IsPaused Nothing))
                    |> loopback
                ]
              )

            Command (ForkFrom idx playingAfter) ->
              ( { state |
                    sessionState <- Forking idx True
                },
                [ API.forkFrom state.session 0
                    |> Task.mapError (\_ -> Debug.crash "...")
                    |> Task.map (Response << HasForked)
                    |> loopback
                ]
              )

            Command (GetNodeState interval nodeIds) ->
              ( { state |
                    sessionState <- Pausing
                }
              , [ (API.setPlaying state.session False
                    |> Task.mapError (\_ -> Debug.crash "...")
                    |> Task.map (always <| Response <| IsPaused <| Just interval)
                    |> loopback)
                  `Task.andThen` (\_ ->
                    -- TODO: factor this out
                    API.getNodeState state.session interval nodeIds
                      |> Task.mapError (\_ -> Debug.crash "...")
                      |> Task.map (Response << GotNodeState)
                      |> loopback
                  )
                ]
              )

            Command (Swap mod) ->
              (state, [])

            _ ->
              Debug.crash "unexpected action in playing state"

    Paused pausedIdx maybeCommand ->
      case maybeCommand of
        Just cmdOut ->
          case cmdOut of
            Swapping ->
              case action of
                Response (SwapResult res) ->
                  (state, [])

                _ ->
                  Debug.crash "..."

            GettingNodeState interval ->
              case action of
                Response (GotNodeState values) ->
                  ( { state
                        | mainVal <-
                            getMainValFromLogs state.session values
                        , sessionState <-
                            Paused interval.start Nothing
                    }
                  , []
                  )

                _ ->
                  Debug.crash "..."

            Subscribing subbing ->
              -- TODO: factor out SUB
              case action of
                Response (IsSubscribed maybeLog) ->
                  case (subbing, maybeLog) of
                    (True, Just valLog) ->
                      (state, [])

                    (False, Nothing) ->
                      (state, [])

                    _ ->
                      Debug.crash "..."

                _ ->
                  Debug.crash "..."

        Nothing ->
          case action of
            Command (Subscribe nodeId sub) ->
              (state, [])

            Command (Swap mod) ->
              (state, [])

            Command (GetNodeState interval nodes) ->
              ( { state |
                    sessionState <-
                        Paused pausedIdx (Just <| GettingNodeState interval)
                }
              , [ API.getNodeState state.session interval nodes
                    |> Task.mapError (\_ -> Debug.crash "...")
                    |> Task.map (Response << GotNodeState)
                    |> loopback
                ]
              )

            Command (ForkFrom frameIdx playAfter) ->
              ( {state | sessionState <- Forking frameIdx playAfter }
              , [ API.forkFrom state.session frameIdx
                    |> Task.mapError (\msg -> Debug.crash msg)
                    |> Task.map (\vals -> Response (HasForked vals))
                    |> loopback
                ]
              )

            _ ->
              Debug.crash "..."

    Forking idx playingAfter ->
      case action of
        Response (HasForked values) ->
          -- TODO: get main, etc...
          ( { state
                | sessionState <-
                    if playingAfter then
                      AlmostPlaying
                    else
                      Paused idx Nothing
                , mainVal <- getMainVal state.session values
            }
          , if playingAfter then
              [ API.setPlaying state.session True
                  |> Task.mapError (\_ -> Debug.crash "already playing")
                  |> Task.map (always <| Response IsPlaying)
                  |> loopback
              ]
            else
              []
          )

        _ ->
          Debug.crash "unexpected action in Forking state"

    Pausing ->
      case action of
        Response (IsPaused maybeInt) ->
          let
            cmdOut =
              maybeInt |> Maybe.map GettingNodeState
          in
            ( { state | sessionState <- Paused (curFrameIdx state) cmdOut }
            , []
            )

        _ ->
          Debug.crash "..."

    AlmostPlaying ->
      case action of
        Response IsPlaying ->
          ( { state | sessionState <- Playing Nothing }
          , []
          )

        _ ->
          Debug.crash "unexpected action in playing state"

    SwapError _ ->
      case action of
        _ ->
          -- TODO: you can reset...
          Debug.crash "action in SwapError state"
