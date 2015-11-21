module Explorer.Value.TreeView where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model a =
  { expansionModel : ExpansionModel
  , tree : Tree a
  }


type Tree a
  = Node a (List (Tree a))
  | Leaf a


type ExpansionModel
  = ExpNode Bool (List ExpansionModel) -- True = expanded
  | ExpLeaf


{- item, expanded. Always false for leaf nodes. -}
type alias RenderFun a =
  a -> Bool -> Html



-- UPDATE


type Action
  = ChangeExpansion Bool
  | ChildAction Int Action


update : Action -> Model a -> Model a
update msg state =
  case msg of
    ChangeExpansion expanded ->
      case (state.tree, state.expansionModel) of
        (Node _ _, ExpNode _ expChildren) ->
          { state | expansionModel = ExpNode expanded expChildren }

        (Leaf _, ExpLeaf) ->
          Debug.crash "trying to change expansion of a leaf"

        _ ->
          Debug.crash "mismatched expansion model and tree"

    ChildAction idx msg ->
      case (state.tree, state.expansionModel) of
        (Node _ children, ExpNode expanded expChildren) ->
          let
            subUpdate childIdx (subTree, subExpModel) =
              if childIdx == idx then
                update msg { tree = subTree, expansionModel = subExpModel }
              else
                { tree = subTree, expansionModel = subExpModel }

            newChildModels =
              List.map2 (,) children expChildren
                |> List.indexedMap subUpdate
          in
            { state | expansionModel =
                ExpNode expanded (List.map .expansionModel newChildModels)
            }

        (Leaf _, ExpLeaf) ->
          Debug.crash "child Action on a leaf"

        _ ->
          Debug.crash "mismatched expansion model and tree"



-- VIEW


(=>) = (,)


view : Signal.Address Action -> RenderFun a -> Model a -> Html
view addr render state =
  case (state.tree, state.expansionModel) of
    (Leaf item, ExpLeaf) ->
      render item False

    (Node item children, ExpNode expanded expChildren) ->
      let
        heading =
          div
            [ style
                [ "display" => "flex"
                , "align-items" => "center"
                ]
            ]
            [ div
                [ onClick addr (ChangeExpansion (not expanded))
                , style
                    [ "cursor" => "default"
                    , "font-size" => "13px"
                    , "width" => "13px"
                    , "height" => "13px"
                    , "padding-right" => "5px"
                    , "-webkit-user-select" => "none"
                    , "color" => "grey"
                    ]
                ]
                [ text <| if expanded then "▼" else "▶" ]
            , div [] [ render item expanded ]
            ]

        childrenViews =
          if expanded then
            [ div []
                [ ul
                    [ style
                        [ "list-style" => "none"
                        , "margin-top" => "0"
                        , "padding-left" => "20px"
                        ]
                    ]
                    (List.map2 (,) children expChildren
                      |> List.indexedMap (\idx (child, expChild) ->
                        li
                          []
                          [ view
                              (Signal.forwardTo addr (ChildAction idx))
                              render
                              { tree = child
                              , expansionModel = expChild
                              }
                          ]
                      )
                    )
                ]
              ]
          else
            []
      in
        div [] ([heading] ++ childrenViews)

    _ ->
      Debug.crash "mismatched tree and expansion model"


allExpanded : Tree a -> ExpansionModel
allExpanded tree =
  case tree of
    Leaf _ ->
      ExpLeaf

    Node _ children ->
      children
        |> List.map allExpanded
        |> ExpNode True


allCollapsed : Tree a -> ExpansionModel
allCollapsed tree =
  case tree of
    Leaf _ ->
      ExpLeaf

    Node _ children ->
      children
        |> List.map allCollapsed
        |> ExpNode False


emptyExpansionModel : ExpansionModel
emptyExpansionModel =
  ExpLeaf
