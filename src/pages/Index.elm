module Index exposing (..) -- where

import Color exposing (Color, darkGrey)
import Dict
import TempFontAwesome as FA
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Markdown
import String



-- MAIN


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { pwd : List String
    , dirs : List String
    , files : List (String, Bool)
    , pkg : Maybe PackageInfo
    , readme : Maybe String
    }


type alias PackageInfo =
    { version : String
    , repository : String
    , summary : String
    , dependencies : List (String, String)
    }


init : Model -> (Model, Cmd msg)
init model =
  (model, Cmd.none)


update : msg -> Model -> (Model, Cmd msg)
update _ model =
  (model, Cmd.none)


subscriptions : Model -> Sub msg
subscriptions model =
  Sub.none



-- CONSTANTS


(=>) = (,)


pageWidth =
  "960px"


smallBoxWidth =
  "300px"


largeBoxWidth =
  "600px"


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


clearfix : Html msg
clearfix =
  div [ style [ "clear" => "both" ] ] []



-- VIEW


view : Model -> Html msg
view model =
  let
    rightColumnView pkgInfo =
      [ div
          [ style ("width" => smallBoxWidth :: "padding-bottom" => "80px" :: floatRight)
          ]
          [ viewPackageInfo pkgInfo
          , dependenciesView pkgInfo.dependencies
          ]
      ]

    contents =
      navigator model.pwd
      :: leftColumnView model
      :: Maybe.withDefault [] (Maybe.map rightColumnView model.pkg)
      ++ [ clearfix ]
  in
    div []
      [ pageHeader model
      , div
          [ style
              [ "width" => pageWidth
              , "margin-left" => "auto"
              , "margin-right" => "auto"
              ]
          ]
          contents
      ]


pageHeader : Model -> Html msg
pageHeader model =
  header
    [ style
        [ "width" => "100%"
        , "background-color" => "#60B5CC"
        , "height" => "8px"
        ]
    ]
    []


leftColumnView : Model -> Html msg
leftColumnView model =
  let
    files =
      div
        [ style (boxStyles ++ ["margin-bottom" => "30px"]) ]
        (
          div [ style <| boxHeaderStyles ++ blockStyles ] [ text "File Navigation" ]
          :: List.map folderDisplay (List.sort (List.filter (not << String.startsWith ".") model.dirs))
          ++ List.map fileDisplay (List.sortBy fst (List.filter (not << String.startsWith "." << fst) model.files))
        )

    viewReadme markdown =
      [ div
          [ style boxStyles ]
          [ div [ style (blockStyles ++ boxHeaderStyles) ] [ text "README" ]
          , Markdown.toHtml [ style (blockStyles ++ boxItemStyles) ] markdown
          ]
      ]
  in
    section
      [ style (floatLeft ++ ["width" => largeBoxWidth, "padding-bottom" => "80px"])
      ]
      (files :: Maybe.withDefault [] (Maybe.map viewReadme model.readme))


folderDisplay : String -> Html msg
folderDisplay folder =
  div
    [ style (blockStyles ++ boxItemStyles)
    ]
    [ a [ href folder ] [ folderIcon, text folder ]
    ]


elmFileLinks : Bool -> String -> List (Html msg)
elmFileLinks isElmFile file =
  let
    jumpLinkStyle =
      floatRight ++
        [ "padding" => "0 5px"
        , "color" => "#c7c7c7"
        ]
  in
    if isElmFile then
      [ a [ href (file ++ "?debug"), style jumpLinkStyle ]
          [ text "debug" ]
      ]

    else
      []


fileDisplay : (String, Bool) -> Html msg
fileDisplay (file, hasMain) =
  div [ style (blockStyles ++ boxItemStyles) ]
    (
      a [ href file
        ]
        [ getIcon file
        , text file
        ]
      :: elmFileLinks hasMain file
    )


navigator : List String -> Html msg
navigator pathSegments =
  let
    hrefs =
      List.scanl (\sub path -> path </> sub) "" ("" :: pathSegments)
        |> List.drop 1

    names =
      FA.home darkGrey 32 :: List.map text pathSegments

    toLink name path =
      a [ href path ] [ name ]

    subfolders =
      List.map2 toLink names hrefs
  in
    div
      [ style [ "font-size" => "2em", "padding" => "20px 0", "display" => "flex", "align-items" => "center", "height" => "40px" ] ]
      (List.intersperse navigatorSeparator subfolders)


navigatorSeparator =
  span [ style [ "padding" => "0 8px" ] ] [ text "/" ]



-- DEPENDENCIES


packageUrl : String -> String -> String
packageUrl name version =
  "http://package.elm-lang.org/packages" </> name </> version


dependenciesView : List (String, String) -> Html msg
dependenciesView dependencies =
  div
    [ style (boxStyles ++ floatRight ++ [ "width" => smallBoxWidth ])
    ]
    ( div
        [ style (boxHeaderStyles ++ blockStyles)
        ]
        [ text "Dependencies" ]
      :: List.map dependencyView dependencies
    )


dependencyView : (String, String) -> Html msg
dependencyView (name, version) =
  div
    [ style (blockStyles ++ boxItemStyles)
    ]
    [ div
        [ style floatLeft ]
        [ packageIcon
        , a [ href (packageUrl name version) ] [ text name ]
        ]
    , div
        [ style floatRight ]
        [ text version ]
    ]


viewPackageInfo : PackageInfo -> Html msg
viewPackageInfo pkgInfo =
  div
    [ style <| boxStyles ++ floatRight ++
        [ "margin-bottom" => "30px"
        , "width" => smallBoxWidth
        ]
    ]
    [ div [ style <| boxHeaderStyles ++ blockStyles ] [ text "Package Information" ]
    , div [ style <| blockStyles ++ boxItemStyles ] [ text pkgInfo.summary ]
    , div [ style <| blockStyles ++ boxItemStyles ] [ text <| "Version: " ++ pkgInfo.version ]
    ]



-- ICONS


getIcon : String -> Html msg
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


makeIcon : (Color -> Int -> Html msg) -> Html msg
makeIcon icon =
  span
    [ style [ "display" => "inline-block", "vertical-align" => "middle", "padding-right" => "0.5em" ] ]
    [ icon darkGrey 16 ]



-- HELPERS


(</>) : String -> String -> String
(</>) directory file =
  if String.endsWith "/" directory then
    directory ++ file

  else
    directory ++ "/" ++ file


takeExtension : String -> String
takeExtension str =
  last (String.split "." str)


last : List String -> String
last list =
  case list of
    [] ->
      ""

    [finalElement] ->
      finalElement

    _ :: rest ->
      last rest

