module Watches where

import Color
import Text
import Text (..)
import Graphics.Element (..)
import List
import Html
import Html (..)
import Html.Attributes as Attributes
import VirtualDom (property)
import VirtualDom
import Json.Encode as Json


watchLabel : String -> Html
watchLabel k = 
    let
        labelProps = [ Attributes.style [ ( "margin-left", "10px" ) 
                                        , ( "font", "14px" )
                                        , ( "font-weight", "bold" )
                                        , ( "font-family", "Gotham,Futura, Lucida Grande, sans-serif")
                                        ] 
                     ]
        n = node "div" labelProps [Html.text k]
    in n

watchValue : String -> Html
watchValue v = 
    let
        valueProps = [ Attributes.style [ ( "margin-left", "20px" ) 
                                        , ( "font", "12px" )
                                        , ( "font-family", "Menlo for Powerline, monospace" )
                                        ] 
                     ]
        n = node "pre" valueProps [Html.text v]
    in n

showWatch' : (String, String) -> Html
showWatch' (k,v) =
    let label = watchLabel k
        value = watchValue v
        color = Attributes.style [ ( "color", "rgb(228, 228, 228)" ) ]
    in node "div" [color] [label, value]

--
-- View
--

view : (Int, Int) -> List (String, String) -> Element
view (w, h) watches = 
    let 
        addScrollBar = [ Attributes.style [ ( "overflow-y", "auto")
                                          , ( "overflow-x", "hidden")
                                          , ( "height", "100%")
                                          ]
                       ]

        watchView = case watches of
                      [] -> noWatches
                      ws -> VirtualDom.toElement w h (node "div" addScrollBar (List.map showWatch' ws))

    in watchView



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
