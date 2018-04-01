module View.Main exposing (view)

import Annotation
import Element exposing (Element)
import Element.Attributes as Attributes exposing (fill)
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
    in
    Element.column Style.None
        [ Attributes.height fill ]
        [ View.ActionBar.deviceActionBar actionBarParameters
        , View.ImageAnnotations.imageViewer model.viewer model.image model.toolsData
        ]
