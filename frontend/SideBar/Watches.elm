module SideBar.Watches where

import Dict as D
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Styles exposing (..)
import SideBar.Controls as Controls


sidePadding =
  20


view : D.Dict String String -> Html
view watches =
  div
    [ style
        [ "overflow-y" => "auto"
        , "overflow-x" => "hidden"
        , "padding" => ("0 " ++ intToPx sidePadding)
        , "position" => "absolute"
        , "bottom" => "10px"
        , "top" => intToPx (Controls.totalHeight + 1)
        , "width" => intToPx (sidebarWidth - 2*sidePadding)
        ]
    ]
    ( if D.isEmpty watches then
        [noWatches]
      else
        watches |> D.toList |> List.map viewWatch
    )


-- WATCHES

viewWatch : (String, String) -> Html
viewWatch (name, value) =
  div watchAttributes [viewName name, viewValue value]


watchAttributes : List Attribute
watchAttributes =
  [ style
    [ "color" => colorToCss Color.lightGrey
    , "padding" => "10px 0"
    ]
  ]


viewName : String -> Html
viewName name =
  div nameAttributes [ text name ]


nameAttributes : List Attribute
nameAttributes =
  [ style
    [ "margin" => "10px 0 10px"
    , "font-weight" => "bold"
    , "font-family" => textTypefaces
    ]
  ]


viewValue : String -> Html
viewValue value =
  pre valueAttributes [ text value ]


valueAttributes : List Attribute
valueAttributes =
  [ style
    [ "margin" => "0 0 0 10px"
    ]
  ]


-- NO WATCHES


noWatches : Html
noWatches =
  div
    [ style
        [ "font-family" => textTypefaces
        , "color" => "rgb(170, 170, 170)" -- dark grey
        ]
    ]
    [ Markdown.toHtml noWatchText ]


noWatchText : String
noWatchText = """

### <span style="font-size: 12pt"> You don't have any watches! </span>

<span style="font-size: 10pt">
Use [<span style="text-decoration:underline; color: rgb(170, 170, 170)">Debug.watch</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watch)
to show any value. <br>
`watch : String -> a -> a`

<span style="font-size: 10pt">
Use [<span style="text-decoration:underline; color: rgb(170, 170, 170)">Debug.watchSummary</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watchSummary) to show a
summary or subvalue of any value. </span><br>

"""
