-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ActionBar exposing (Msg, allProvided, configProvided, imagesProvided, nothingProvided)

import Data.AnnotatedImage exposing (AnnotatedImage)
import Data.Feature as Feature exposing (Feature)
import Data.Tool as Tool exposing (Tool)
import Element exposing (Element)
import Element.Background
import Element.Events
import Element.Font
import Html exposing (Html)
import Html.Attributes
import Json.Encode as Encode exposing (Value)
import Packages.FileInput as FileInput
import Packages.Zipper as Zipper exposing (Zipper)
import View.Icon as Icon
import View.Style as Style



-- TYPES #############################################################


type alias Msg msg =
    { loadImages : List { name : String, file : Value } -> msg
    , loadConfig : Value -> msg

    -- messages available when an image is provided
    , zoomIn : msg
    , zoomOut : msg
    , zoomFit : ( Float, Float ) -> msg

    -- below are only for AllProvided state
    , selectTool : Int -> msg
    , removeAnnotation : msg
    , saveAnnotations : msg
    }



-- FUNCTIONS #########################################################


nothingProvided : Msg msg -> Element msg
nothingProvided msg =
    Element.row [ Element.alignRight ]
        [ Element.text "Load images →"
        , loadImagesButton msg.loadImages
        ]


imagesProvided : Msg msg -> Element msg
imagesProvided msg =
    Element.row [ Element.alignRight ]
        [ Element.text "Load config →"
        , loadConfigButton msg.loadConfig
        , loadImagesButton msg.loadImages
        ]


configProvided : Msg msg -> List Feature -> Zipper Tool -> Element msg
configProvided msg features toolsZipper =
    Element.row [ Element.width Element.fill ]
        [ Element.row [] (List.map toolButtonDisabled (Zipper.getAll toolsZipper))
        , filler
        , zoomButtonsDisabled features
        , removeAnnotationButtonDisabled features
        , filler
        , saveAnnotationsButtonDisabled
        , loadConfigButton msg.loadConfig
        , loadImagesButton msg.loadImages
        ]


allProvided : Msg msg -> AnnotatedImage -> List Feature -> Zipper Tool -> Element msg
allProvided msg annotatedImage features toolsZipper =
    let
        ( zoomButtons, removeButton, saveButton ) =
            case annotatedImage.status of
                Data.AnnotatedImage.Loaded image ->
                    ( zoomButtonsAbled msg ( image.width, image.height ) features
                    , removeAnnotationButtonDisabled features
                    , saveAnnotationsButtonDisabled
                    )

                Data.AnnotatedImage.LoadedWithAnnotations image _ _ ->
                    ( zoomButtonsAbled msg ( image.width, image.height ) features
                    , removeAnnotationButtonAbled msg.removeAnnotation features
                    , saveAnnotationsButtonAbled msg.saveAnnotations
                    )

                _ ->
                    ( zoomButtonsDisabled features
                    , removeAnnotationButtonDisabled features
                    , saveAnnotationsButtonDisabled
                    )

        toolsButtons =
            List.concat
                [ List.map (toolButtonAbled msg.selectTool) (Zipper.getL toolsZipper)
                , [ toolButtonFocused (Zipper.getC toolsZipper) ]
                , List.map (toolButtonAbled msg.selectTool) (Zipper.getR toolsZipper)
                ]
    in
    Element.row [ Element.width Element.fill ]
        [ Element.row [] toolsButtons
        , filler
        , zoomButtons
        , removeButton
        , filler
        , saveButton
        , loadConfigButton msg.loadConfig
        , loadImagesButton msg.loadImages
        ]



--


zoomButtonsAbled : Msg msg -> ( Int, Int ) -> List Feature -> Element msg
zoomButtonsAbled msg imageSize features =
    featureElement Feature.CanZoom features <|
        Element.row []
            [ zoomInButtonAbled msg.zoomIn
            , zoomOutButtonAbled msg.zoomOut
            , zoomFitButtonAbled (msg.zoomFit (Tuple.mapBoth toFloat toFloat imageSize))
            ]


zoomButtonsDisabled : List Feature -> Element msg
zoomButtonsDisabled features =
    featureElement Feature.CanZoom features <|
        Element.row []
            [ zoomInButtonDisabled
            , zoomOutButtonDisabled
            , zoomFitButtonDisabled
            ]


zoomInButtonAbled : msg -> Element msg
zoomInButtonAbled zoomInMsg =
    abledButton zoomInMsg "Zoom in" (Icon.toHtml 60 Icon.zoomIn)


zoomInButtonDisabled : Element msg
zoomInButtonDisabled =
    disabledButton "Zoom in" (Icon.toHtml 60 Icon.zoomIn)


zoomOutButtonAbled : msg -> Element msg
zoomOutButtonAbled zoomOutMsg =
    abledButton zoomOutMsg "Zoom out" (Icon.toHtml 60 Icon.zoomOut)


zoomOutButtonDisabled : Element msg
zoomOutButtonDisabled =
    disabledButton "Zoom out" (Icon.toHtml 60 Icon.zoomOut)


zoomFitButtonAbled : msg -> Element msg
zoomFitButtonAbled zoomFitMsg =
    abledButton zoomFitMsg "Fit image" (Icon.toHtml 60 Icon.zoomFit)


zoomFitButtonDisabled : Element msg
zoomFitButtonDisabled =
    disabledButton "Fit image" (Icon.toHtml 60 Icon.zoomFit)



--


saveAnnotationsButtonAbled : msg -> Element msg
saveAnnotationsButtonAbled saveAnnotationsMsg =
    abledButton saveAnnotationsMsg "Save annotations" (Icon.toHtml 60 Icon.download)


saveAnnotationsButtonDisabled : Element msg
saveAnnotationsButtonDisabled =
    disabledButton "Save annotations" (Icon.toHtml 60 Icon.download)



--


removeAnnotationButtonAbled : msg -> List Feature -> Element msg
removeAnnotationButtonAbled removeAnnotationMsg features =
    abledButton removeAnnotationMsg "Remove this annotation" (Icon.toHtml 60 Icon.trash2)
        |> featureElement Feature.CanRemoveAnnotation features


removeAnnotationButtonDisabled : List Feature -> Element msg
removeAnnotationButtonDisabled features =
    disabledButton "Remove this annotation" (Icon.toHtml 60 Icon.trash2)
        |> featureElement Feature.CanRemoveAnnotation features



--


featureElement : Feature -> List Feature -> Element msg -> Element msg
featureElement feature features element =
    if List.member feature features then
        element

    else
        Element.none



--


toolButtonFocused : Tool -> Element msg
toolButtonFocused tool =
    focusedButton (toolTitle tool) (svgToolIcon tool)


toolButtonAbled : (Int -> msg) -> Tool -> Element msg
toolButtonAbled selectToolMsg tool =
    abledButton (selectToolMsg (Tool.toId tool)) (toolTitle tool) (svgToolIcon tool)


toolButtonDisabled : Tool -> Element msg
toolButtonDisabled tool =
    disabledButton (toolTitle tool) (svgToolIcon tool)


toolTitle : Tool -> String
toolTitle tool =
    case tool of
        Tool.Move ->
            "Grab and move the image"

        Tool.Point ->
            "Points"

        Tool.BBox ->
            "Bounding boxes"

        Tool.Line ->
            "Free lines"

        Tool.Outline ->
            "Outlines"

        Tool.Polygon ->
            "Polygons"


svgToolIcon : Tool -> Html msg
svgToolIcon tool =
    case tool of
        Tool.Move ->
            Icon.toHtml 60 Icon.move

        Tool.Point ->
            Icon.toHtml 60 Icon.point

        Tool.BBox ->
            Icon.toHtml 60 Icon.boundingBox

        Tool.Line ->
            Icon.toHtml 60 Icon.line

        Tool.Outline ->
            Icon.toHtml 60 Icon.outline

        Tool.Polygon ->
            Icon.toHtml 60 Icon.polygon



--


abledButton : msg -> String -> Html msg -> Element msg
abledButton msg title icon =
    Html.div centerFlexAttributes [ icon ]
        |> Element.html
        |> Element.el
            [ Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
            , Element.pointer
            , Element.Events.onClick msg
            , Element.htmlAttribute (Html.Attributes.title title)
            ]


disabledButton : String -> Html msg -> Element msg
disabledButton title icon =
    Html.div centerFlexAttributes [ icon ]
        |> Element.html
        |> Element.el
            [ Element.Font.color Style.disabledText
            , Element.htmlAttribute (Html.Attributes.title title)
            ]


focusedButton : String -> Html msg -> Element msg
focusedButton title icon =
    Html.div centerFlexAttributes [ icon ]
        |> Element.html
        |> Element.el
            [ Element.Background.color Style.focusedItemBG
            , Element.htmlAttribute (Html.Attributes.title title)
            ]



--


filler : Element msg
filler =
    Element.el [ Element.width Element.fill ] Element.none



--


loadConfigButton : (Value -> msg) -> Element msg
loadConfigButton loadConfigMsg =
    let
        uniqueId =
            "load-config"

        icon =
            [ Icon.toHtml 60 Icon.settings ]
                |> Html.label (iconLabelAttributes uniqueId)
                |> Element.html
                |> Element.el
                    [ Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
                    , Element.htmlAttribute (Html.Attributes.title "Load JSON file configuration")
                    ]

        invisibleInput =
            FileInput.invisible
                { id = uniqueId
                , accept = "application/json"
                , quantity = FileInput.SingleWith loadConfigMsg
                }
    in
    Element.row [] [ icon, Element.html invisibleInput ]


loadImagesButton : (List { name : String, file : Value } -> msg) -> Element msg
loadImagesButton loadImagesMsg =
    let
        uniqueId =
            "load-images"

        icon =
            [ Icon.toHtml 60 Icon.image ]
                |> Html.label (iconLabelAttributes uniqueId)
                |> Element.html
                |> Element.el
                    [ Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
                    , Element.htmlAttribute (Html.Attributes.title "Load images")
                    ]

        invisibleInput =
            FileInput.invisible
                { id = uniqueId
                , accept = "image/*"
                , quantity = FileInput.MultipleWith loadImagesMsg
                }
    in
    Element.row [] [ icon, Element.html invisibleInput ]


iconLabelAttributes : String -> List (Html.Attribute msg)
iconLabelAttributes uniqueId =
    -- need to manually add a cursor because the class given by elm-ui
    -- gets overwritten by user agent stylesheet for a label
    Html.Attributes.for uniqueId
        :: Html.Attributes.style "cursor" "pointer"
        :: centerFlexAttributes


centerFlexAttributes : List (Html.Attribute msg)
centerFlexAttributes =
    [ Html.Attributes.style "width" "100px"
    , Html.Attributes.style "height" "100px"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "justify-content" "center"
    ]
