module Index where

import Color exposing (Color, darkGrey)
import Dict
import FontAwesome as FA
import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import String
import Util exposing (..)


-- MAIN

main : Html
main =
  view info


port info : Model


-- MODEL

type alias Model =
    { pwd : List String
    , dirs : List String
    , files : List String
    , description : Maybe Description
    }


type alias Description =
    { version : String
    , repository : String
    , summary : String
    , license : String
    }


-- CONSTANTS

(=>) = (,)


pageWidth = "960px"
smallBoxWidth = "300px"
largeBoxWidth = "600px"

padding = "20px"


floatLeft =
  [ "float" => "left" ]


floatRight =
  [ "float" => "right" ]


boxStyles =
  [ "border" => "1px solid #c7c7c7"
  , "border-radius" => "5px"
  ]


boxHeaderStyles =
  [ "background-color" => "#fafafa"
  , "text-align" => "center"
  ]


blockStyles =
  [ "display" => "block"
  , "overflow" => "hidden"
  , "padding" => "7px 12px"
  ]


boxItemStyles =
  [ "border-top" => "1px solid #e1e1e1" ]


linkStyles =
  [ "color" => "#1184ce"
  , "text-decoration" => "none"
  ]


clearfix : Html
clearfix =
  div [ style [ "clear" => "both" ] ] []


-- VIEW

view : Model -> Html
view model =
  let
    packageDependants cpackage =
      [ div
        [ style <| "width" => smallBoxWidth :: "margin-top" => "30px" :: floatRight ]
        [ descriptionDisplay cpackage
        --, dependenciesView cpackage.dependencies
        ]
      ]

    contents =
      folderView model
      :: Maybe.withDefault [] (Maybe.map packageDependants model.description)
      ++ [ clearfix ]
  in
    div
      [ style
          [ "font-family" => """"Open Sans", "Arial", sans-serif"""
          , "margin" => "0"
          , "padding" => "0"
          , "background-color" => "white"
          ]
      ]
      [ pageHeader model
      , div
          [ style
              [ "width" => pageWidth
              , "padding" => padding
              , "margin-left" => "auto"
              , "margin-right" => "auto"
              ]
          ]
          contents
      ]



pageHeader : Model -> Html
pageHeader model =
  header
    [ style
        [ "width" => "100%"
        , "background-color" => "#1184ce"
        , "font-weight" => "bold"
        , "color" => "white"
        , "box-shadow" => "0 0 7px #47474"
        ]
    ]
    [ div
        [ style
            [ "padding" => padding
            ]
        ]
        [ div
            [ style floatLeft
            ]
            [ text "Elm Reactor" ]
        , clearfix
        ]
    ]


folderView : Model -> Html
folderView model =
  section
    [ style floatLeft ]
    [ h2 [] (navigator model.pwd)
    , div
        [ style <| boxStyles ++ floatLeft ++ [ "width" => largeBoxWidth ] ]
        (
          div [ style <| boxHeaderStyles ++ blockStyles ] [ text "File Navigation" ]
          :: List.map folderDisplay (List.sort model.dirs)
          ++ List.map fileDisplay (List.sort model.files)
        )
    ]


folderDisplay : String -> Html
folderDisplay folder =
  a [ href folder
    , style (linkStyles ++ blockStyles ++ boxItemStyles)
    ]
    [ folderIcon, text folder
    ]


elmFileLinks : Bool -> String -> List Html
elmFileLinks isElmFile file =
  let
    jumpLinkStyle =
      linkStyles ++ floatRight ++
        [ "padding" => "0 5px"
        , "color" => "#c7c7c7"
        ]
  in
    if isElmFile then
      [ a [ href (file ++ "?debug"), style jumpLinkStyle ]
          [ text "Debug" ]
      ]
    else
      []


fileDisplay : String -> Html
fileDisplay file =
  let
    isElmFile = String.endsWith ".elm" file
  in
    div
      [ style <| blockStyles ++ boxItemStyles ]
      <| [ a
        [ href <| file
        , style linkStyles
        ]
        [ getIcon file
        , span [ style [ "display" => "inline-block"
                       , "width" => if isElmFile then "75%" else "90%"
                       ]
               ] [ text file ]
        ]
      ] ++ (elmFileLinks isElmFile file)


navigator : List String -> List Html
navigator pathSegments =
  let
    hrefs =
      List.scanl (\sub path -> path </> sub) "" ("" :: pathSegments)

    names =
      List.map text pathSegments

    toLink name path =
      a [ href path, style linkStyles ] [ name ]

    subfolders =
      List.map2 toLink names hrefs
  in
    List.intersperse guiPathSeparator subfolders


guiPathSeparator =
  span [ separatorStyle ] [ text "/" ]


separatorStyle =
  style
    [ "display" => "inline-block"
    , "margin" => "0 5px"
    ]


{--

packageUrl : Dependency -> String
packageUrl {account, name, version} =
  "http://package.elm-lang.org/packages"
  </> account
  </> name
  </> version


accountUrl : Dependency -> String
accountUrl {account} = "https://github.com" </> account


dependenciesView : List Dependency -> Html
dependenciesView dependencies =
  div
    [ style <| boxStyles ++ floatRight ++
        [ "width" => smallBoxWidth ]
    ]
    (div [ style <| boxHeaderStyles ++ blockStyles
         ] [ text "Dependencies" ] ::
      List.map dependencyView dependencies)


dependencyView : Dependency -> Html
dependencyView package =
  let
    {account, name, version} = package
  in
    div
      [ style <| blockStyles ++ boxItemStyles ]
      [ div
        [ style floatLeft ]
        [ packageIcon
        , a [ href <| accountUrl package, style linkStyles ] [ text account ]
        , guiDependencySeparator
        , a [ href <| packageUrl package, style linkStyles ] [ text name ]
        ]
      , div
        [ style floatRight ]
        [ text version ]
      ]

guiDependencySeparator = span [ separatorStyle ] [ text "/" ]

--}

descriptionDisplay : Description -> Html
descriptionDisplay {version, summary, repository} =
  div
    [ style <| boxStyles ++ floatRight ++
        [ "margin-bottom" => "30px"
        , "width" => smallBoxWidth
        ]
    ]
    [ div [ style <| boxHeaderStyles ++ blockStyles ] [ text "Package Information" ]
    , div [ style <| blockStyles ++ boxItemStyles ] [ text summary ]
    , div [ style <| blockStyles ++ boxItemStyles ] [ text <| "Package Version: " ++ version ]
    , div [ style <| blockStyles ++ boxItemStyles ] [ text repository ]
    ]


-- ICONS

getIcon : String -> Html
getIcon filename =
  let
    file = String.toLower filename
  in
    Dict.get (takeExtension file) endings
      |> Maybe.withDefault fileIcon


endings =
  Dict.fromList
    [ "jpg"  => imageIcon
    , "jpeg" => imageIcon
    , "png"  => imageIcon
    , "gif"  => imageIcon
    ]


imageIcon =
  makeIcon FA.file_image_o


fileIcon =
  makeIcon FA.file_text_o


folderIcon =
  makeIcon FA.folder


packageIcon =
  makeIcon FA.archive


makeIcon : (Color -> Int -> Html) -> Html
makeIcon icon =
  span
    [ style [ "padding" => "2px 6px 0 0" ] ]
    [ icon darkGrey 14 ]


