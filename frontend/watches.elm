module Watches where

import Color
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Element as GE
import Graphics.Input (..)
import List
import Signal
import Signal (Signal, (<~), (~))
import Slider (..)
import Text
import Text (..)
import Window

-- Style
--

panelWidth = 275

dataStyle : List String -> Float -> String -> Text
dataStyle typefaces height string =
    let myStyle =
             { defaultStyle
             | typeface <- typefaces
             , color <- Color.lightGrey
             , height <- Just height
             }
    in
        style myStyle (Text.fromString string)

watchStyle : String -> Text
watchStyle = dataStyle ["Gotham", "Futura", "Lucida Grande", "sans-serif"] 14

codeStyle : String -> Text
codeStyle = dataStyle ["Menlo for Powerline", "monospace"] 12

--
-- View
--

watchView : (Int, Int) -> List (String, String) -> Element
watchView (w, h) watches = 
    let showWatch (k,v) = flow down
            [ k |> watchStyle |> bold |> leftAligned |> width w
            , v |> codeStyle |> leftAligned |> width w
            , spacer 1 12
            ]

        watchView = flow right
            [ spacer 20 1
            , case watches of
                [] -> noWatches
                ws -> List.map showWatch ws |> flow down
            ]
    in watchView


--
-- The wiring
--

main : Signal Element
main = watchView <~ ((\(w, h) -> (panelWidth, h)) <~ Window.dimensions)
                  ~ watches

port watches : Signal (List (String, String))

--
-- Copy
--

noWatches : Element
noWatches = [markdown|

### <span style="font-family: Gotham, Futura, 'Lucida Grande', sans-serif; font-size: 12pt; color: rgb(170,170,170)"> You don't have any watches! </span>

<span style="color: rgb(170,170,170)">
<span style="font-family: Gotham, Futura, 'Lucida Grande', sans-serif; font-size: 10pt; color: rgb(170,170,170)">
Use [<span style="text-decoration:underline; color: rgb(170,170,170)">Debug.watch</span>](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Debug#watch)
to show any value. <br>
`watch : String -> a -> a`</span>

<span style="font-family: Gotham, Futura, 'Lucida Grande', sans-serif; font-size: 10pt; color: rgb(170,170,170)">
Use [<span style="text-decoration:underline; color: rgb(170,170,170)">Debug.watchSummary</span>](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Debug#watchSummary) to show a <br>
summary or subvalue of any value. </span><br>
|]


