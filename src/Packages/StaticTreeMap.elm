module Packages.StaticTreeMap exposing (..)

import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias Foldable a =
    { key : Int
    , folded : Bool
    , item : a
    }


type alias StaticTreeMap a =
    Tree (Foldable a)


from : Tree a -> StaticTreeMap a
from tree =
    Tree.indexedMap (\id a -> { key = id, folded = True, item = a }) tree


type alias StaticTreeMapZipper a =
    Zipper (Foldable a)


foldedTree : Int -> StaticTreeMap a -> StaticTreeMap a
foldedTree targetKey treeMap =
    Zipper.fromTree treeMap
        |> subFold targetKey
        |> Maybe.map Zipper.tree
        |> Maybe.withDefault treeMap


subFold : Int -> StaticTreeMapZipper a -> Maybe (StaticTreeMapZipper a)
subFold targetKey zipper =
    let
        label =
            Zipper.label zipper

        key =
            label.key

        unfoldedZipper =
            Zipper.replaceLabel { label | folded = False } zipper
    in
    if key == targetKey then
        unfoldedZipper
            |> Zipper.lastChild
            |> Maybe.map purgeZipper
            |> Maybe.andThen purgePreviousSiblings
            |> Maybe.withDefault unfoldedZipper
            |> purgePreviousSiblings
    else if key < targetKey then
        case Zipper.lastChild unfoldedZipper of
            Nothing ->
                Nothing

            Just lastChild ->
                subFold targetKey lastChild
    else
        case Zipper.previousSibling (purgeZipper zipper) of
            Nothing ->
                Nothing

            Just previousSibling ->
                subFold targetKey previousSibling


purgeZipper : StaticTreeMapZipper a -> StaticTreeMapZipper a
purgeZipper zipper =
    case Zipper.children zipper of
        [] ->
            Zipper.mapLabel (\label -> { label | folded = False }) zipper

        _ ->
            Zipper.mapLabel (\label -> { label | folded = True }) zipper
                |> Zipper.mapTree (Tree.replaceChildren [])


purgePreviousSiblings : StaticTreeMapZipper a -> Maybe (StaticTreeMapZipper a)
purgePreviousSiblings zipper =
    case Zipper.previousSibling zipper of
        Nothing ->
            case Zipper.parent zipper of
                Nothing ->
                    Just zipper

                Just parent ->
                    purgePreviousSiblings parent

        Just previousSibling ->
            purgeZipper previousSibling
                |> purgePreviousSiblings



-- viewLabel : (Int -> msg) -> Int -> Foldable String -> Html msg
-- viewLabel msgTagger selectedKey foldableItem =
--     let
--         itemKey =
--             foldableItem.key
--
--         attributes =
--             if itemKey == selectedKey then
--                 []
--             else
--                 [ onClick (msgTagger itemKey) ]
--
--         textContent =
--             if foldableItem.folded then
--                 "+ " ++ foldableItem.item
--             else
--                 foldableItem.item
--     in
--     span attributes [ text textContent ]
--
--
-- view : Model -> Html Msg
-- view model =
--     let
--         foldedTree =
--             StaticTreeMap.foldedTree model.selected model.treeFromRoot
--
--         labelToHtml =
--             StaticTreeMap.viewLabel Select model.selected
--
--         toListItems : Html msg -> List (Html msg) -> Html msg
--         toListItems htmlLabel children =
--             Html.li []
--                 [ htmlLabel
--                 , Html.ul [] children
--                 ]
--     in
--     Tree.restructure labelToHtml toListItems foldedTree
--         |> (\root -> Html.ul [] [ root ])
