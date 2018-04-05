module View.Main exposing (view)

import Annotation
import Class
import Dataset
import Element exposing (Element, below, column, el, empty)
import Element.Attributes as Attributes exposing (alignLeft, alignRight, fill, paddingTop)
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
        currentImageData =
            Zipper.getC model.imagesData

        hasImage =
            case currentImageData of
                Loaded _ _ _ _ ->
                    True

                _ ->
                    False

        hasAnnotation =
            case currentImageData of
                Loaded _ _ _ toolsData ->
                    case .tool (Zipper.getC toolsData) of
                        Tool.Move ->
                            False

                        Tool.Annotation annotations ->
                            Annotation.hasAnnotation annotations

                _ ->
                    False

        toolsData =
            case currentImageData of
                Loaded _ _ _ toolsData ->
                    toolsData

                _ ->
                    Zipper.init [] (Tool.Data 0 Tool.Move 0) []

        actionBarParameters =
            { device = model.device
            , size = model.layout.actionBarSize
            , canClearAnnotations = hasAnnotation
            , hasImage = hasImage
            , toolsData = toolsData
            }

        classesView =
            Class.viewAll SelectClass model.classesData.selectedKey model.classesData.classes
                |> el Style.ClassesSidebar [ alignLeft, paddingTop 10 ]

        datasetView =
            Dataset.viewAll SelectImage model.imagesData
                |> el Style.ClassesSidebar [ alignRight, paddingTop 10 ]
    in
    Element.column Style.None
        [ Attributes.height fill ]
        [ View.ActionBar.deviceActionBar actionBarParameters
            |> below [ classesView ]
            |> below [ datasetView ]
        , View.ImageAnnotations.imageViewer
            model.viewer
            model.classesData.selectedKey
            currentImageData
        ]
