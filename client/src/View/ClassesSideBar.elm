-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ClassesSideBar exposing (view)

import Element exposing (Element, column, el, empty, text)
import Element.Attributes as Attributes exposing (padding, paddingLeft)
import Packages.StaticTreeMap as StaticTreeMap exposing (Foldable, StaticTreeMap)
import Pointer
import StyleSheet as Style exposing (Style)
import Tree


view : (Int -> msg) -> { selected : Int, all : StaticTreeMap String } -> Element Style var msg
view selectClassMsg { selected, all } =
    StaticTreeMap.foldedTree selected all
        |> Tree.children
        |> List.map (Tree.restructure (viewClass selectClassMsg selected) toListItems)
        |> column Style.None []


toListItems : Element Style var msg -> List (Element Style var msg) -> Element Style var msg
toListItems classElement children =
    case children of
        [] ->
            classElement

        _ ->
            column Style.None
                []
                [ classElement
                , column Style.None [ paddingLeft 40 ] children
                ]


viewClass : (Int -> msg) -> Int -> Foldable String -> Element Style var msg
viewClass selectClassMsg selectedClassId foldableItem =
    let
        itemKey =
            foldableItem.key

        attributes =
            if itemKey == selectedClassId then
                [ padding 10 ]

            else
                [ padding 10
                , Pointer.onDown (always <| selectClassMsg itemKey)
                    |> Attributes.toAttr
                ]

        textContent =
            if foldableItem.folded then
                text ("+ " ++ foldableItem.item)

            else
                text foldableItem.item
    in
    if itemKey == 0 then
        empty

    else if itemKey == selectedClassId then
        el (Style.ClassItem Style.SelectedClass) attributes textContent

    else
        el (Style.ClassItem Style.NonSelectedClass) attributes textContent
