module Overlay where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal
import Color

import Button
import Styles exposing (..)
import Model
import OverlayModel
import SideBar.Controls as Controls
import SideBar.Watches as Watches


view : Signal.Address Model.Action -> Model.Model -> Html
view addr state =
  div
    []
    [ sidebar addr state
    , eventBlocker (Model.isPaused state)
    ]


-- TODO: errors!


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


sidebar : Signal.Address Model.Action -> Model.Model -> Html
sidebar addr state =
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
      if state.overlayModel.sidebarVisible
      then [ "right" => "0" ]
      else [ "right" => intToPx -sidebarWidth ]

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

    overlayActions =
      Signal.forwardTo addr Model.OverlayAction

    contents =
      case state.attachmentState of
        -- TODO: prettify
        Model.Uninitialized ->
          [ text "Uninitialized" ]

        Model.Swapping _ ->
          [ text "Swapping" ]

        Model.Connected connectedAttrs ->
          [ Controls.view addr overlayActions state
          , dividerBar
          , Watches.view connectedAttrs.currentWatches
          ]

  in
    div
      [ id "elm-reactor-side-bar"
      -- done in JS: cancelBubble / stopPropagation on this
      , style (constantStyles ++ toggleStyles)
      ]
      [ toggleTab overlayActions state.overlayModel
      , div [] contents
      ]


tabWidth = 25

toggleTab : Signal.Address OverlayModel.Action -> OverlayModel.Model -> Html
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
    , onClick
        addr
        (OverlayModel.SidebarVisible <| not state.sidebarVisible)
    ]
    []
