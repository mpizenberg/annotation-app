-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ActionBar
    exposing
        ( Parameters
        , emptyView
        , responsiveHeight
        , viewAll
        , viewConfig
        , viewImages
        )

import Data.Tool as Tool exposing (Tool)
import Element exposing (Element, el, empty)
import Element.Attributes as Attributes exposing (fill, height, px, toAttr, vary, width)
import Html.Attributes
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Decode exposing (Decoder, Value)
import Packages.Button as Button
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (ColorVariations, Style)
import Svg exposing (Svg)
import View.Icon as Icon



-- TYPES #############################################################


type alias Parameters msg =
    { size : ( Float, Float )
    , hasAnnotations : Bool
    , mturkMode : Bool

    -- events
    , removeLatestAnnotationMsg : msg
    , selectToolMsg : Int -> msg
    , zoomInMsg : msg
    , zoomOutMsg : msg
    , zoomFitMsg : msg
    , loadConfigMsg : Value -> msg
    , loadImagesMsg : List { name : String, file : Value } -> msg
    , exportMsg : msg
    }



-- FUNCTIONS #########################################################


responsiveHeight : Device -> Int
responsiveHeight device =
    case ( device.kind, device.orientation ) of
        ( Device.Phone, Device.Portrait ) ->
            device.size.width // 7

        ( Device.Phone, Device.Landscape ) ->
            device.size.width // 13

        ( Device.Tablet, Device.Portrait ) ->
            device.size.width // 10

        ( Device.Tablet, Device.Landscape ) ->
            device.size.width // 16

        _ ->
            min 72 (device.size.width // 16)


emptyView : Parameters msg -> Element Style ColorVariations msg
emptyView params =
    let
        ( w, h ) =
            params.size

        filler =
            el Style.None [ width fill, height (px h) ] empty

        instruction =
            textButton h False params.exportMsg "Load images →"
    in
    [ filler, instruction, datasetButton params.loadImagesMsg h ]
        |> Element.row Style.None []


viewImages : Parameters msg -> Element Style ColorVariations msg
viewImages params =
    let
        ( w, h ) =
            params.size

        filler =
            el Style.None [ width fill, height (px h) ] empty

        instruction =
            textButton h False params.exportMsg "Load Json config →"
    in
    filler
        :: instruction
        :: configButton params.loadConfigMsg h
        :: [ datasetButton params.loadImagesMsg h ]
        |> Element.row Style.None []


viewConfig : Parameters msg -> Zipper Tool -> Element Style ColorVariations msg
viewConfig params tools =
    let
        ( w, h ) =
            params.size

        filler =
            el Style.None [ width fill, height (px h) ] empty

        toolButtons =
            List.concat
                [ Zipper.getL tools
                    |> List.map (disabledToolButton h)
                , [ disabledToolButton h (Zipper.getC tools) ]
                , Zipper.getR tools
                    |> List.map (disabledToolButton h)
                ]

        configAndDatasetButtons =
            [ configButton params.loadConfigMsg h
            , datasetButton params.loadImagesMsg h
            ]
    in
    (toolButtons ++ filler :: configAndDatasetButtons)
        |> Element.row Style.None []


viewAll : Parameters msg -> Zipper Tool -> Element Style ColorVariations msg
viewAll params tools =
    let
        ( w, h ) =
            params.size

        filler =
            el Style.None [ width fill, height (px h) ] empty

        toolButtons =
            List.concat
                [ Zipper.getL tools
                    |> List.map (toolButton params.selectToolMsg h False)
                , [ toolButton params.selectToolMsg h True (Zipper.getC tools) ]
                , Zipper.getR tools
                    |> List.map (toolButton params.selectToolMsg h False)
                ]

        removeLatestButton =
            actionButton h params.hasAnnotations params.removeLatestAnnotationMsg "Delete latest annotation of current tool" Icon.trash2

        optionsButtons =
            if params.mturkMode then
                [ textButton h True params.exportMsg "Submit" ]

            else
                [ actionButton h True params.exportMsg "Save annotations" Icon.save
                , configButton params.loadConfigMsg h
                , datasetButton params.loadImagesMsg h
                ]

        zoomActions =
            [ actionButton h True params.zoomInMsg "Zoom in" Icon.zoomIn
            , actionButton h True params.zoomOutMsg "Zoom out" Icon.zoomOut
            , actionButton h True params.zoomFitMsg "Fit zoom to image" Icon.zoomFit
            ]
    in
    (toolButtons ++ filler :: removeLatestButton :: filler :: zoomActions ++ filler :: optionsButtons)
        |> Element.row Style.None []


datasetButton : (List { name : String, file : Value } -> msg) -> Float -> Element Style ColorVariations msg
datasetButton loadImagesMsg h =
    Button.loadMultipleFilesInput "Load images"
        { msgTagger = loadImagesMsg
        , uniqueId = "image-loader"
        , innerElement = Element.html (lazy2 Icon.toHtml (0.6 * h) Icon.image)
        , size = h
        , noStyle = Style.None
        , outerStyle = Style.Button Style.Abled
        }


configButton : (Value -> msg) -> Float -> Element Style ColorVariations msg
configButton loadConfigMsg h =
    Button.loadFileInput "Load JSON config file"
        { msgTagger = loadConfigMsg
        , uniqueId = "config-loader"
        , innerElement = Element.html (lazy2 Icon.toHtml (0.6 * h) Icon.settings)
        , size = h
        , noStyle = Style.None
        , outerStyle = Style.Button Style.Abled
        }


toolButton : (Int -> msg) -> Float -> Bool -> Tool -> Element Style ColorVariations msg
toolButton selectToolMsg size isSelected tool =
    Button.view
        { actionability =
            if isSelected then
                Button.Abled Button.Active

            else
                Button.Abled Button.Inactive
        , action =
            Pointer.onDown (always <| selectToolMsg tool.id)
                |> Attributes.toAttr
        , innerElement = toolIcon (0.6 * size) tool.variant tool.type_
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if isSelected then
                Style.Button Style.Selected

            else
                Style.Button Style.Abled
        , otherAttributes = []
        }


disabledToolButton : Float -> Tool -> Element Style ColorVariations msg
disabledToolButton size tool =
    Button.view
        { actionability = Button.Disabled
        , action = Attributes.class ""
        , innerElement = toolIcon (0.6 * size) tool.variant tool.type_
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle = Style.Button Style.Disabled
        , otherAttributes = []
        }


toolIcon : Float -> Int -> Tool.Type -> Element Style ColorVariations msg
toolIcon size variant type_ =
    let
        ( svgIcon, tooltipText ) =
            case type_ of
                Tool.Move ->
                    ( Icon.move, "Move" )

                Tool.Point ->
                    ( Icon.point, "Point" )

                Tool.BBox ->
                    ( Icon.boundingBox, "Bounding box" )

                Tool.Stroke ->
                    ( Icon.stroke, "Stroke" )

                Tool.Outline ->
                    ( Icon.outline, "Outline" )

                Tool.Polygon ->
                    ( Icon.polygon, "Polygon" )
    in
    lazy2 Icon.toHtml size svgIcon
        |> Element.html
        |> el Style.ToolIcon [ vary (Style.FromPalette variant) True, toAttr <| Html.Attributes.title tooltipText ]


actionButton : Float -> Bool -> msg -> String -> List (Svg msg) -> Element Style ColorVariations msg
actionButton size clickable sendMsg tooltipText innerSvg =
    Button.view
        { actionability =
            if clickable then
                Button.Abled Button.Inactive

            else
                Button.Disabled
        , action = Pointer.onDown (always sendMsg) |> Attributes.toAttr
        , innerElement = Element.html (lazy2 Icon.toHtml (0.6 * size) innerSvg)
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if clickable then
                Style.Button Style.Abled

            else
                Style.Button Style.Disabled
        , otherAttributes = [ toAttr <| Html.Attributes.title tooltipText ]
        }


textButton : Float -> Bool -> msg -> String -> Element Style ColorVariations msg
textButton height clickable sendMsg innerText =
    Button.viewText
        { actionability =
            if clickable then
                Button.Abled Button.Inactive

            else
                Button.Disabled
        , action = Pointer.onDown (always sendMsg) |> Attributes.toAttr
        , innerElement = Element.text innerText
        , innerStyle = Style.None
        , height = height
        , outerStyle =
            if clickable then
                Style.TextButton Style.Abled

            else
                Style.TextButton Style.Disabled
        , otherAttributes = []
        }
