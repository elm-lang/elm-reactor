module SideBar.Footer where

import Color
import FontAwesome
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import String

import Debugger.Active as Active
import Model
import SideBar.Button as Button
import Html.File as File
import Utils.Style exposing ((=>))


styles : String
styles = """

.left-sidebar-footer {
  background-color: darkgray;

  -webkit-flex: 0 0 auto;
  -moz-flex: 0 0 auto;
  -ms-flex: 0 0 auto;
  flex: 0 0 auto;

  background-color: rgb(46, 46, 46);

  font-family: monospace;
  color: rgb(99, 99, 99);
}

.swap-status {
  text-align: center;
  padding-top: 8px;
}

.import-export-buttons {
  display: flex;
  justify-content: space-around;
}

.import-export-button {
  cursor: pointer;
  text-decoration: underline;
  padding: 8px 0;

  transition: all 0.2s ease 0s;
}

.import-export-button:hover {
  color: rgb(164, 164, 164);
}

#import-file-chooser {
  position: fixed;
  top: -10000px;
  left: -10000px;
}

"""

type alias Handlers =
  { changeFile : (List File.File -> Signal.Message)
  , clickExport : Signal.Message
  , clickImport : (String -> Signal.Message)
  }


-- VIEW

view : Handlers -> String -> Html
view handlers status =
  div
    [ class "left-sidebar-footer" ]
    [ div [ class "swap-status" ] [ text status ]
    , exportImport handlers
    ]


exportImport : Handlers -> Html
exportImport handlers =
  div
    [ class "import-export-buttons" ]
    [ span
        [ class "import-export-button"
        , on "click" (succeed ()) (\_ -> handlers.clickExport)
        ]
        [ text "export session" ]

    , span
        [ class "import-export-button"
        , on "click" (succeed ()) (\_ -> handlers.clickImport "#import-file-chooser")
        ]
        [ text "import session" ]

    , input
        [ id "import-file-chooser"
        , type' "file"
        , accept "application/json"
        , on
            "change"
            (at ["target", "files"] <| File.domList File.file)
            handlers.changeFile
        ]
        []
    ]
