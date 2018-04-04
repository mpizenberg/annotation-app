module Class exposing (..)

import Annotation
import Element exposing (Element, below, column, el, empty, row, text)
import Element.Attributes as Attributes exposing (alignLeft, fill, toAttr)
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
                []
            else
                [ toAttr <| Pointer.onDown (always <| msgTagger itemKey) ]
    in
    if foldableItem.folded then
        el Style.None attributes (text foldableItem.item)
    else
        el Style.None attributes (text foldableItem.item)


viewAll : (Int -> msg) -> Int -> Classes -> Element Style var msg
viewAll msgTagger selectedKey classes =
    let
        foldedClasses =
            StaticTreeMap.foldedTree selectedKey classes

        classView =
            view msgTagger selectedKey

        toListItems classElement children =
            column Style.None
                []
                [ classElement
                , column Style.None [] children
                ]
    in
    Tree.restructure classView toListItems foldedClasses
