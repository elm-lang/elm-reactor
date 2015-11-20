module ElmValueView where

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as JsEnc
import Set exposing (Set)
import String

import Debugger.Reflect as Reflect
import StartApp.Simple as StartApp
import TreeView


type alias Model =
  { val : Reflect.ElmValue
  , expansion : Expansion
  }


-- TODO: use from List.Extra
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
  case list of
      [] ->
          Nothing

      first::rest ->
          if predicate first then
              Just first
          else
              find predicate rest


type Expansion
  = NotExpandable
  | ListExpansion (Dict Int Expansion)
  | ArrayExpansion (Dict Int Expansion)
  | DictExpansion (List (Reflect.ElmValue, Expansion, Expansion))
  | SetExpansion (List (Reflect.ElmValue, Expansion))
  | RecordExpansion (Dict String Expansion)
  | ConstructorExpansion String (Dict Int Expansion)


reconcile : Reflect.ElmValue -> Expansion -> Expansion
reconcile newValue previousExpansion =
  let
    byIndex : Dict Int Expansion -> List Reflect.ElmValue -> Dict Int Expansion
    byIndex expandedIndices values =
      values
        |> List.indexedMap (,)
        |> List.filterMap (\(idx, val) ->
              expandedIndices
                |> Dict.get idx
                |> Maybe.map (\oldExp -> (idx, reconcile val oldExp)))
        |> Dict.fromList
  in
    case (previousExpansion, newValue) of
      (NotExpandable, _) ->
        NotExpandable

      (ListExpansion expandedIndices, Reflect.ListV values) ->
        byIndex expandedIndices values
          |> ListExpansion

      (ArrayExpansion expandedIndices, Reflect.ArrayV values) ->
        byIndex expandedIndices values
          |> ArrayExpansion

      (DictExpansion expansion, Reflect.DictV keyValuePairs) ->
        let
          findOldExpansion : Reflect.ElmValue -> Maybe (Expansion, Expansion)
          findOldExpansion key =
            expansion
              |> find (\(oldKey, keyExp, valueExp) -> key == oldKey)
              |> Maybe.map (\(_, keyExp, valueExp) -> (keyExp, valueExp))
        in
          keyValuePairs
            |> List.filterMap (\(key, value) ->
                findOldExpansion key
                  |> Maybe.map (\(keyExp, valueExp) ->
                      (key, reconcile key keyExp, reconcile value valueExp)))
            |> DictExpansion

      (SetExpansion expandedValues, Reflect.SetV values) ->
        values
          |> List.filterMap (\value ->
              find (\(expValue, expansion) -> expValue == value) expandedValues)
          |> SetExpansion

      (RecordExpansion expandedFields, Reflect.Record fieldsAndValues) ->
        fieldsAndValues
          |> List.filterMap (\(field, newFieldVal) ->
                expandedFields
                  |> Dict.get field
                  |> Maybe.map (\oldFieldExp ->
                        (field, reconcile newFieldVal oldFieldExp)))
          |> Dict.fromList
          |> RecordExpansion

      (ConstructorExpansion oldName expandedArgIndices, Reflect.Constructor newName args) ->
        if newName == oldName then
          byIndex expandedArgIndices args
            |> ConstructorExpansion oldName
        else
          ConstructorExpansion newName Dict.empty

      (_, _) ->
        Debug.crash <|
          "mismatched expansion model & value: " ++
            (toString (previousExpansion, newValue))


initModel =
  let
    val =
      { x = 2
      , y = "Foo"
      , z = [Just 1,Nothing,Just 3]
      , a = Dict.fromList [('a', 3), ('b', 4)]
      , b = Set.fromList [1,2,3]
      , d = Array.fromList [1,2,3]
      , e = Just "sup"
      , f = (1,2,5)
      , g = (Just (Just 4), Just Nothing)
      }
      |> Reflect.reflect
      |> Debug.log "val"

    asTree =
      toTree val

  in
    { val = val
    , expansion = TreeView.allExpanded asTree
    }


app : StartApp.Config Model TreeView.Action
app =
  { model = initModel
  , view = view
  , update = update
  }


main =
  StartApp.start app


update : TreeView.Action -> Model -> Model
update msg state =
  let
    newTreeViewState =
      TreeView.update msg { tree = toTree state.val, expansion = state.expansion }
  in
    { state | expansion = newTreeViewState.Expansion }


view : Signal.Address TreeView.Action -> Model -> Html
view addr state =
  let
    tree =
      toTree state.val
  in
    TreeView.view addr render { tree = tree, expansion = state.expansion }


type ValueView
  = ContainerHeading Reflect.ElmValue
  | OrderedItem Int ValueView
  | RecordItem String ValueView
  | DictItemHeading Reflect.ElmValue Reflect.ElmValue
  | DictKey ValueView
  | DictValue ValueView
  | AtomValue Reflect.ElmValue


toTree : Reflect.ElmValue -> TreeView.Tree ValueView
toTree elmVal =
  let
    -- TODO dedupe
    mkOrdItem idx val =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (OrderedItem idx view)

        TreeView.Node view children ->
          TreeView.Node
            (OrderedItem idx view)
            children

    mkRecordItem (name, val) =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (RecordItem name view)

        TreeView.Node view children ->
          TreeView.Node
            (RecordItem name view)
            children

    mkDictKey val =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (DictKey view)

        TreeView.Node view children ->
          TreeView.Node
            (DictKey view)
            children

    mkDictVal val =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (DictValue view)

        TreeView.Node view children ->
          TreeView.Node
            (DictValue view)
            children

  in
    case elmVal of
      Reflect.ListV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem items)

      Reflect.DictV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (items |> List.map (\(k, v) ->
            TreeView.Node
              (DictItemHeading k v)
              [ mkDictKey k
              , mkDictVal v
              ]
          ))

      Reflect.SetV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem items)

      Reflect.ArrayV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem items)

      Reflect.TupleV args ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem args)

      Reflect.Constructor ctor args ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem args)

      Reflect.Record items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.map mkRecordItem items)

      _ ->
        elmVal
          |> AtomValue
          |> TreeView.Leaf


(=>) = (,)


colorText : String -> String -> Html
colorText color str =
  span
    [ style ["color" => color] ]
    [ text str ]


render : TreeView.RenderFun ValueView
render valueView expanded =
  let
    core =
      case valueView of
        ContainerHeading val ->
          case val of
            Reflect.ListV items ->
              text <| "List (" ++ toString (List.length items) ++ ")"

            Reflect.DictV items ->
              text <| "Dict (" ++ toString (List.length items) ++ ")"

            Reflect.SetV items ->
              text <| "Set " ++ toString (List.length items)

            Reflect.ArrayV items ->
              text <| "Array (" ++ toString (List.length items) ++ ")"

            Reflect.TupleV items ->
              text <| "(" ++ String.join ", " (List.repeat (List.length items) "…") ++ ")"

            Reflect.Record items ->
              span []
                ( [ text "{" ]
                  ++ List.intersperse
                      (text ", ")
                      (items |> List.map (\(name, val) ->
                        span []
                          [ colorText "purple" (name ++ "=")
                          , text "…"
                          ]
                      ))
                  ++ [ text "}" ]
                )

            Reflect.Constructor ctor items ->
              text ctor

            _ ->
              Debug.crash <| "not a container: " ++ (toString val)

        OrderedItem idx view ->
          span []
            [ colorText "purple" <| toString idx ++ ": "
            , render view expanded
            ]

        RecordItem name view ->
          span []
            [ colorText "purple" name
            , colorText "darkgrey" " = "
            , render view expanded
            ]

        DictItemHeading keyVal valVal ->
          text "…"

        DictKey view ->
          span []
            [ colorText "purple" "key: "
            , render view expanded
            ]

        DictValue view ->
          span []
            [ colorText "purple" "val: "
            , render view expanded
            ]

        AtomValue val ->
          case val of
            Reflect.NumberV int ->
              colorText "blue" <| toString int

            Reflect.CharV chr ->
              colorText "green" <| "'" ++ String.fromChar chr ++ "'"

            Reflect.StringV str ->
              -- TODO add slashes
              colorText "green" <| "\"" ++ str ++ "\""

            Reflect.BoolV bool ->
              colorText "blue" <| if bool then "True" else "False"

            _ ->
              Debug.crash <| "not an atom: " ++ toString val
  in
    span [ style ["font-family" => "monospace"] ] [ core ]
