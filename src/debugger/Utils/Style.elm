module Utils.Style where

import Color
import String


(=>) = (,)


sidebarWidth = 275


lightGrey = Color.rgb 228 228 228
darkGrey = Color.rgb 74 74 74


textTypefaces : String
textTypefaces =
  "Gotham, Futura, 'Lucida Grande', sans-serif"


-- TODO: put this in Html.Attributes?
colorToCss : Color.Color -> String
colorToCss color =
  let
    record =
      Color.toRgb color

    colors =
      List.map toString [record.red, record.green, record.blue]

    numbers =
      String.join "," (colors ++ [toString record.alpha])

    in
      "rgba(" ++ numbers ++ ")"


intToPx : Int -> String
intToPx x =
  toString x ++ "px"


unselectable : List (String, String)
unselectable =
  [ "-webkit-user-select" => "none"
  , "-moz-user-select" => "none"
  , "-ms-user-select" => "none"
  ]


-- TODO: would be nice to have these in the Color lib
{-| See https://github.com/mbostock/d3/wiki/Colors#rgb_brighter -}
brighter : Float -> Color.Color -> Color.Color
brighter factor color =
  multClampChannels (30, 255) color -factor


{-| See https://github.com/mbostock/d3/wiki/Colors#rgb_darker -}
darker : Float -> Color.Color -> Color.Color
darker factor color =
  multClampChannels (0, 255) color factor


multClampChannels : (Float, Float) -> Color.Color -> Float -> Color.Color
multClampChannels (cMin, cMax) color factor =
  let
    record =
      Color.toRgb color

    multClamp x =
      round <| clamp cMin cMax ((toFloat x) * 0.7 ^ factor)
  in
    Color.rgba
      (multClamp record.red)
      (multClamp record.green)
      (multClamp record.blue)
      record.alpha
