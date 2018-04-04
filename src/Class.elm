module Class exposing (..)

import Annotation
import Element exposing (Element, below, column, el, empty, row, text)
import Element.Attributes as Attributes exposing (alignLeft, fill, padding, paddingLeft, spacing, toAttr)
import Packages.StaticTreeMap as StaticTreeMap exposing (Foldable, StaticTreeMap)
import Pointer
import StyleSheet as Style exposing (Style)
import Tree


type alias Classes =
    StaticTreeMap String


fromConfig : Annotation.Config -> Classes
fromConfig config =
    let
        convertNode : Annotation.ClassesConfig -> ( String, List Annotation.ClassesConfig )
        convertNode classes =
            case classes of
                Annotation.ClassItem className ->
                    ( className, [] )

                Annotation.ClassCategory categoryName subClasses ->
                    ( categoryName, subClasses )
    in
    Annotation.ClassCategory "classes" config.classes
        |> Tree.unfold convertNode
        |> StaticTreeMap.from


view : (Int -> msg) -> Int -> Foldable String -> Element Style var msg
view msgTagger selectedKey foldableItem =
    let
        itemKey =
            foldableItem.key

        attributes =
            if itemKey == selectedKey then
                [ padding 10 ]
            else
                [ padding 10, toAttr <| Pointer.onDown (always <| msgTagger itemKey) ]

        textContent =
            if foldableItem.folded then
                text ("+ " ++ foldableItem.item)
            else
                text foldableItem.item
    in
    if itemKey == 0 then
        empty
    else if itemKey == selectedKey then
        el (Style.ClassItem Style.SelectedClass) attributes textContent
    else
        el (Style.ClassItem Style.NonSelectedClass) attributes textContent


viewAll : (Int -> msg) -> Int -> Classes -> Element Style var msg
viewAll msgTagger selectedKey classes =
    let
        foldedClasses =
            StaticTreeMap.foldedTree selectedKey classes

        classView =
            view msgTagger selectedKey

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
    in
    Tree.children foldedClasses
        |> List.map (Tree.restructure classView toListItems)
        |> column Style.None []
