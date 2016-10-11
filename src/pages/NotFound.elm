module NotFound exposing (main)

import Html exposing (..)
import Html.Attributes exposing (style)


(=>) = (,)


main : Html msg
main =
  div
    [ style
        [ "width" => "100%"
        , "height" => "100%"
        , "display" => "flex"
        , "flex-direction" => "column"
        , "justify-content" => "center"
        , "align-items" => "center"
        , "background-color" => "#F5F5F5"
        , "color" => "#9A9A9A"
        ]
    ]
    [ div [ style ["font-size" => "12em"] ] [ text "404" ]
    , div [ style ["font-size" => "3em"] ] [ text "Page not found" ]
    ]
