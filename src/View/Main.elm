module View.Main exposing (view)

import Annotation
import Class
import Element exposing (Element, below, column, el, empty)
import Element.Attributes as Attributes exposing (alignLeft, fill)
import Html exposing (Html)
import Packages.Zipper as Zipper
import StyleSheet as Style exposing (Style)
import Tool
import Types exposing (..)
import View.ActionBar
import View.ImageAnnotations


view : Model -> Html Msg
view model =
    responsiveLayout model
        |> Element.layout Style.sheet


responsiveLayout : Model -> Element Style Style.ColorVariations Msg
responsiveLayout model =
    let
        hasAnnotation =
            case .tool (Zipper.getC model.toolsData) of
                Tool.Move ->
                    False

                Tool.Annotation annotations ->
                    Annotation.hasAnnotation annotations

        actionBarParameters =
            { device = model.device
            , size = model.layout.actionBarSize
            , canClearAnnotations = hasAnnotation
            , hasImage = model.image /= Nothing
            , toolsData = model.toolsData
            }

        classesView =
            Class.viewAll SelectClass model.classesData.selectedKey model.classesData.classes
                |> el Style.None [ alignLeft ]
    in
    Element.column Style.None
        [ Attributes.height fill ]
        [ View.ActionBar.deviceActionBar actionBarParameters
            |> below [ classesView ]
        , View.ImageAnnotations.imageViewer model.viewer model.image model.toolsData
        ]
