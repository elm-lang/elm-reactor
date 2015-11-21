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
}

.import-export-button {
  cursor: pointer;
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
    [ text status
    , exportImport handlers
    ]


exportImport : Handlers -> Html
exportImport handlers =
  div
    [ style
        [ "display" => "flex"
        , "justify-content" => "space-around"
        , "text-decoration" => "underline"
        ]
    ]
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
