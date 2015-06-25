module Styles where

import Color
import String


textTypefaces : String
textTypefaces =
  "Gotham, Futura, 'Lucida Grande', sans-serif"


-- TODO: put this in Html.Attributes?
colorToCss : Color.Color -> String
colorToCss color =
  let
    record =
      Color.toRgb color

    rgb =
      List.map toString [record.red, record.green, record.blue]

    numbers =
      String.join "," (rgb ++ [toString record.alpha])

  in
    "rgba(" ++ numbers ++ ")"


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
