module Index where


import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import String
import Util exposing (..)
import Dict exposing (fromList)
import Maybe.Extra exposing (or)


-- CONSTANTS

(=>) = (,)

iconPath = "open-iconic/png"
separatorStyle = style [ "display" => "inline-block", "margin" => "0 5px" ]
guiDependencySeparator = span [ separatorStyle ] [ text "/" ]
guiPathSeparator = span [ separatorStyle ] [ text "/" ]
standardIconType = ".png"
standardIconSize = "4x"
standardIconDimensions = 14
homeIcon      = flip sizedIcon "home"
homeIconLarge = homeIcon 18
homeIconSmall = homeIcon standardIconDimensions
fileIcon      = basicIcon "file"
folderIcon    = basicIcon "folder"
packageIcon   = basicIcon "book"
htmlIcon      = basicIcon "browser"
imageIcon     = basicIcon "aperture"
codeIcon      = basicIcon "code"
cogIcon       = basicIcon "cog"
wrenchIcon    = basicIcon "wrench"
configIcon    = wrenchIcon
textIcon      = basicIcon "document"
dataIcon      = basicIcon "spreadsheet"
keyIcon       = basicIcon "key"
legalIcon     = keyIcon
lockIcon      = basicIcon "lock-locked"
audioIcon     = basicIcon "audio"
listIcon      = basicIcon "list"
hardDriveIcon = basicIcon "hard-drive"
dataDaseIcon  = hardDriveIcon
terminalIcon  = basicIcon "terminal"

pageWidth     = "960px"
smallBoxWidth = "300px"
largeBoxWidth = "600px"
padding       = "20px"
floatLeft     = [ "float" => "left" ]
floatRight    = [ "float" => "right" ]
boxStyles     = [ "border" => "1px solid #c7c7c7"
                , "border-radius" => "5px"
                ]
boxHeaderStyles = [ "background-color" => "#fafafa"
                  , "text-align" => "center"
                  ]
blockStyles   = [ "display" => "block"
                , "overflow" => "hidden"
                , "padding" => "7px 12px"
                ]
boxItemStyles = [ "border-top" => "1px solid #e1e1e1" ]
linkStyles    = [ "color" => "#1184ce"
                , "text-decoration" => "none"
                ]

endings = fromList
  [
  -- Code
    ("elm", codeIcon)
  , ("hs", codeIcon)
  , ("lhs", codeIcon)
  , ("js", codeIcon)
  , ("php", codeIcon)
  , ("py", codeIcon)
  , ("rb", codeIcon)
  , ("coffee", codeIcon)
  , ("sh", terminalIcon)
  , ("bash", terminalIcon)

  -- Images
  , ("jpg", imageIcon)
  , ("jpeg", imageIcon)
  , ("png", imageIcon)
  , ("gif", imageIcon)

  -- Audio
  , ("mp3", audioIcon)
  , ("wma", audioIcon)
  , ("ogg", audioIcon)
  , ("oga", audioIcon)
  , ("flac", audioIcon)

  -- Data formats
  , ("json", dataIcon)
  , ("yml", dataIcon)
  , ("xml", dataIcon)
  , ("db", dataDaseIcon)
  , ("sqlite", dataDaseIcon)

  -- Text files
  , ("txt", textIcon)
  , ("log", listIcon)

  -- Misc
  , ("md", textIcon)
  , ("", fileIcon)
  , ("html", htmlIcon)
  , ("htm", htmlIcon)
  , ("lock", lockIcon)
  ]


fullFileIcons = fromList
  [ (".gitignore", configIcon)
  , ("elm-package.json", configIcon)
  , ("bower.json", configIcon)
  , ("license", legalIcon)
  , ("license.txt", legalIcon)
  , ("makefile", configIcon)
  ]


clearfix : Html
clearfix =
  div [ style [ "clear" => "both" ] ] []


-- UTILITY FUNCTIONS


packageUrl : Dependency -> String
packageUrl {account, name, version} =
  "http://package.elm-lang.org/packages"
  </> account
  </> name
  </> version


accountUrl : Dependency -> String
accountUrl {account} = "https://github.com" </> account


basicIcon = sizedIcon standardIconDimensions


sizedIcon size name =
  img [ src <| iconPath </> name ++ "-" ++ standardIconSize ++ standardIconType, width size, height size ] []


iconBox : String -> Html -> Html
iconBox position icon =
  let baseStyles =
        [ "padding" => "2px 0" ]
      styles =
        if position == "left"
          then baseStyles ++ [ "padding-right" => "5px" ]
          else baseStyles ++ [ "padding-left" => "5px" ]
  in
    span
      [ style styles ]
      [ icon ]


getIcon : String -> Html
getIcon filename =
  let file = String.toLower filename in
  Maybe.withDefault fileIcon <| Dict.get file fullFileIcons `or` Dict.get (takeExtension file) endings


-- TYPES


type alias Model =
  { currentFolder : String
  , folders       : List String
  , files         : List String
  , currpackage   : Maybe Package
  }


type alias Dependency =
  { name    : String
  , account : String
  , version : String
  }


type alias Package =
  { version : String
  , repository : String
  , summary : String
  , license : String
  , dependencies : List Dependency
  }


-- VIEW

view : Model -> Html
view model =
  let
    packageDependants cpackage =
      [ div
        [ style <| "width" => smallBoxWidth :: "margin-top" => "30px" :: floatRight ]
        [ packageDisplay cpackage
        , dependenciesView cpackage.dependencies
        ]
      ]
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
        [ style [ "width" => pageWidth
                , "padding" => padding
                , "margin-left" => "auto"
                , "margin-right" => "auto"
                ]
        ]
        <| [ folderView model
           ]
           ++ Maybe.withDefault [] (Maybe.map packageDependants model.currpackage)
           ++ [ clearfix ]
      ]



pageHeader : Model -> Html
pageHeader {currentFolder} =
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
      [ style [ "padding" => padding
              ]
      ]
      [ div [ style floatLeft
            ] <| formatSubpathNavigation True homeIconSmall currentFolder
      , clearfix
      ]
    ]


folderView : Model -> Html
folderView {currentFolder, folders, files} =
  section
    [ style floatLeft ]
    [ h2 [] <| formatSubpathNavigation False homeIconLarge currentFolder
    , div
      [ style <| boxStyles ++ floatLeft ++ [ "width" => largeBoxWidth ] ]
      (div [ style <| boxHeaderStyles ++ blockStyles ] [ text "File Navigation" ] ::
        List.map folderDisplay (List.sort folders) ++
          List.map fileDisplay (List.sort files)
      )
    ]


folderDisplay : String -> Html
folderDisplay folder =
  a [ href folder
    , style <| linkStyles ++ blockStyles ++ boxItemStyles
    ] [ iconBox "left" folderIcon, text folder ]

elmFileLinks : Bool -> String -> List Html
elmFileLinks isElmFile file =
  let
    styles =
      linkStyles ++ floatRight ++
        [ "padding" => "0 5px"
        , "color" => "#c7c7c7"
        ]
  in
    if isElmFile
      then [ a [ href <| file ++ "?repl"
               , style styles
               ] [ text "REPL" ]
           , a [ href <| file ++ "?debug"
               , style styles
               ] [ text "Debug" ] ]
      else []

fileDisplay : String -> Html
fileDisplay file =
  let
    isElmFile = ".elm" `isSuffixOf` file
  in
    div
      [ style <| blockStyles ++ boxItemStyles ]
      <| [ a
        [ href <| file
        , style linkStyles
        ]
        [ iconBox "left" <| getIcon file
        , span [ style [ "display" => "inline-block"
                       , "width" => if isElmFile then "75%" else "90%"
                       ]
               ] [ text file ]
        ]
      ] ++ (elmFileLinks isElmFile file)

subpathNavigationStyles : Bool -> List (String, String)
subpathNavigationStyles negative =
  if negative
    then linkStyles ++ [ "color" => "#fafafa" ]
    else linkStyles

formatSubpathNavigation : Bool -> Html -> String -> List Html
formatSubpathNavigation negative home path =
  let
    subfolderNames = splitPath path
    subFolderPaths = List.drop 1 <| List.scanl (flip (</>)) "" <| "" :: subfolderNames
    subfolderNameRepresentation = List.map text subfolderNames
    subfolders = (home :: subfolderNameRepresentation) >< subFolderPaths
  in
    subfolders
      |>List.map (\(name, path) ->
                  a [ href path
                    , style <| subpathNavigationStyles negative
                    ] [ name ])
      |> List.intersperse guiPathSeparator


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
        [ style <| [ "width" => "70%" ] ++ floatLeft ]
        [ iconBox "left" packageIcon
        , a [ href <| accountUrl package, style linkStyles ] [ text account ]
        , guiDependencySeparator
        , a [ href <| packageUrl package, style linkStyles ] [ text name ]
        ]
      , div
        [ style <| [ "width" => "30%" ] ++ floatRight ]
        [ text version ]
      ]


packageDisplay : Package -> Html
packageDisplay {version, summary, repository} =
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


-- SIGNALS


main : Signal Html
main = Signal.constant <| view modelPort


port modelPort : Model
