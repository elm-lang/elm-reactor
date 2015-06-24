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
        List.map toString
          [record.red, record.green, record.blue]
      
      numbers =
        String.join ","
          (rgb ++ [toString record.alpha])
          
    in
      "rgba(" ++ numbers ++ ")"
