module SideBar.Watches where

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


view : List (String, String) -> Html
view watches =
  div viewAttributes <|
      case watches of
        [] -> [noWatches]
        _ -> List.map viewWatch watches


viewAttributes : List Attribute
viewAttributes =
  [ style
    [ ("overflow-y", "auto")
    , ("overflow-x", "hidden")
    , ("height", "100%")
    , ("padding", "0 20px")
    ]
  ]



-- WATCHES

viewWatch : (String, String) -> Html
viewWatch (name, value) =
  div watchAttributes [viewName name, viewValue value]


watchAttributes : List Attribute
watchAttributes =
  [ style
    [ ("color", "rgb(228, 228, 228)")
    ]
  ]


viewName : String -> Html
viewName name = 
  div nameAttributes [ text name ]


nameAttributes : List Attribute
nameAttributes =
  [ style 
    [ ("margin", "20px 0 10px") 
    , ("font-weight", "bold")
    , ("font-family", "Gotham, Futura, 'Lucida Grande', sans-serif")
    ]
  ]


viewValue : String -> Html
viewValue value = 
  pre valueAttributes [ text value ]


valueAttributes : List Attribute
valueAttributes =
  [ style
    [ ("margin", "0 0 0 10px") 
    ] 
  ]


-- NO WATCHES

noWatches : Html
noWatches = Markdown.toHtml """

### <span style="font-family: Gotham, Futura, 'Lucida Grande', sans-serif; font-size: 12pt; color: rgb(170,170,170)"> You don't have any watches! </span>

<span style="color: rgb(170,170,170)">
<span style="font-family: Gotham, Futura, 'Lucida Grande', sans-serif; font-size: 10pt; color: rgb(170,170,170)">
Use [<span style="text-decoration:underline; color: rgb(170,170,170)">Debug.watch</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watch)
to show any value. <br>
`watch : String -> a -> a`</span>

<span style="font-family: Gotham, Futura, 'Lucida Grande', sans-serif; font-size: 10pt; color: rgb(170,170,170)">
Use [<span style="text-decoration:underline; color: rgb(170,170,170)">Debug.watchSummary</span>](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#watchSummary) to show a <br>
summary or subvalue of any value. </span><br>

"""
