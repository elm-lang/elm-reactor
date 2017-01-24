module Index.Icon exposing
  ( image
  , file
  , gift
  , folder
  , package
  , plus
  , lookup
  )

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html, span)
import Html.Attributes exposing (class)

import TempFontAwesome as FA



-- ICONS


image : Html msg
image =
  create FA.file_image_o


file : Html msg
file =
  create FA.file_text_o


gift : Html msg
gift =
  create FA.gift


folder : Html msg
folder =
  create FA.folder


package : Html msg
package =
  create FA.archive


plus : Html msg
plus =
  create FA.plus


create : (Color -> Int -> Html msg) -> Html msg
create icon =
  span [ class "pre-icon" ] [ icon Color.darkGrey 16 ]



-- LOOKUP


lookup : String -> Html msg
lookup fileName =
  let
    extension =
      getExtension fileName
  in
    Maybe.withDefault file (Dict.get extension extensionIcons)


extensionIcons : Dict String (Html msg)
extensionIcons =
  Dict.fromList
    [ "jpg"  => image
    , "jpeg" => image
    , "png"  => image
    , "gif"  => image
    ]


(=>) = (,)


getExtension : String -> String
getExtension str =
  getExtensionHelp (String.split "." str)


getExtensionHelp : List String -> String
getExtensionHelp segments =
  case segments of
    [] ->
      ""

    [ext] ->
      String.toLower ext

    _ :: rest ->
      getExtensionHelp rest
