module View.ActionBar exposing (..)

import Data.Tool as Tool exposing (Tool)
import Element exposing (Element, el, empty)
import Element.Attributes as Attributes exposing (fill, height, px, vary, width)
import Element.Keyed as Keyed
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
    , hasImage : Bool

    -- events
    , removeLatestAnnotationMsg : msg
    , selectToolMsg : Int -> msg
    , zoomInMsg : msg
    , zoomOutMsg : msg
    , zoomFitMsg : msg
    , loadConfigMsg : Value -> msg
    , loadImagesMsg : List { name : String, file : Value } -> msg
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

        actionButtons =
            [ actionButton h params.hasAnnotations params.removeLatestAnnotationMsg Icon.trash2
            , Button.loadFileInput
                { msgTagger = params.loadConfigMsg
                , uniqueId = "config-loader"
                , innerElement = Element.html (Icon.toHtml (0.6 * h) Icon.settings)
                , size = h
                , noStyle = Style.None
                , outerStyle = Style.Button Style.Abled
                }
            , Button.loadMultipleFilesInput
                { msgTagger = params.loadImagesMsg
                , uniqueId = "image-loader"
                , innerElement = Element.html (Icon.toHtml (0.6 * h) Icon.image)
                , size = h
                , noStyle = Style.None
                , outerStyle = Style.Button Style.Abled
                }
            ]

        zoomActions =
            [ actionButton h params.hasImage params.zoomInMsg Icon.zoomIn
            , actionButton h params.hasImage params.zoomOutMsg Icon.zoomOut
            , actionButton h params.hasImage params.zoomFitMsg Icon.zoomFit
            ]
    in
    (actionButtons ++ filler :: zoomActions)
        |> Element.row Style.None []


view : Parameters msg -> Zipper Tool -> Element Style ColorVariations msg
view params tools =
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

        actionButtons =
            [ actionButton h params.hasAnnotations params.removeLatestAnnotationMsg Icon.trash2
            , Button.loadFileInput
                { msgTagger = params.loadConfigMsg
                , uniqueId = "config-loader"
                , innerElement = Element.html (Icon.toHtml (0.6 * h) Icon.settings)
                , size = h
                , noStyle = Style.None
                , outerStyle = Style.Button Style.Abled
                }
            , Button.loadMultipleFilesInput
                { msgTagger = params.loadImagesMsg
                , uniqueId = "image-loader"
                , innerElement = Element.html (Icon.toHtml (0.6 * h) Icon.image)
                , size = h
                , noStyle = Style.None
                , outerStyle = Style.Button Style.Abled
                }
            ]

        zoomActions =
            [ actionButton h params.hasImage params.zoomInMsg Icon.zoomIn
            , actionButton h params.hasImage params.zoomOutMsg Icon.zoomOut
            , actionButton h params.hasImage params.zoomFitMsg Icon.zoomFit
            ]
    in
    (toolButtons ++ filler :: actionButtons ++ filler :: zoomActions)
        |> Element.row Style.None []


viewKeyed : Parameters msg -> Zipper Tool -> Element Style ColorVariations msg
viewKeyed params tools =
    let
        ( w, h ) =
            params.size

        fillerKeyed str =
            ( str, el Style.None [ width fill, height (px h) ] empty )

        toolButtons =
            List.concat
                [ Zipper.getL tools
                    |> List.map (toolButtonKeyed params.selectToolMsg h False)
                , [ toolButtonKeyed params.selectToolMsg h True (Zipper.getC tools) ]
                , Zipper.getR tools
                    |> List.map (toolButtonKeyed params.selectToolMsg h False)
                ]

        actionButtons =
            [ actionButtonKeyed h params.hasAnnotations params.removeLatestAnnotationMsg Icon.trash2
            , ( "load-config"
              , Button.loadFileInput
                    { msgTagger = params.loadConfigMsg
                    , uniqueId = "config-loader"
                    , innerElement = Element.html (Icon.toHtml (0.6 * h) Icon.settings)
                    , size = h
                    , noStyle = Style.None
                    , outerStyle = Style.Button Style.Abled
                    }
              )
            , ( "load-images"
              , Button.loadMultipleFilesInput
                    { msgTagger = params.loadImagesMsg
                    , uniqueId = "image-loader"
                    , innerElement = Element.html (Icon.toHtml (0.6 * h) Icon.image)
                    , size = h
                    , noStyle = Style.None
                    , outerStyle = Style.Button Style.Abled
                    }
              )
            ]

        zoomActions =
            [ actionButtonKeyed h params.hasImage params.zoomInMsg Icon.zoomIn
            , actionButtonKeyed h params.hasImage params.zoomOutMsg Icon.zoomOut
            , actionButtonKeyed h params.hasImage params.zoomFitMsg Icon.zoomFit
            ]
    in
    (toolButtons ++ fillerKeyed "filler1" :: actionButtons ++ fillerKeyed "filler2" :: zoomActions)
        |> Keyed.row Style.None []


toolButtonKeyed : (Int -> msg) -> Float -> Bool -> Tool -> ( String, Element Style ColorVariations msg )
toolButtonKeyed selectToolMsg size isSelected tool =
    ( toString tool.id, toolButton selectToolMsg size isSelected tool )


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


toolIcon : Float -> Int -> Tool.Type -> Element Style ColorVariations msg
toolIcon size variant type_ =
    let
        svgIcon =
            case type_ of
                Tool.Move ->
                    Icon.move

                Tool.Point ->
                    Icon.point

                Tool.BBox ->
                    Icon.boundingBox

                Tool.Stroke ->
                    Icon.stroke

                Tool.Outline ->
                    Icon.outline

                Tool.Polygon ->
                    Icon.polygon
    in
    Icon.toHtml size svgIcon
        |> Element.html
        |> el Style.ToolIcon [ vary (Style.FromPalette variant) True ]


actionButtonKeyed : Float -> Bool -> msg -> List (Svg msg) -> ( String, Element Style ColorVariations msg )
actionButtonKeyed size clickable sendMsg innerSvg =
    ( toString sendMsg, actionButton size clickable sendMsg innerSvg )


actionButton : Float -> Bool -> msg -> List (Svg msg) -> Element Style ColorVariations msg
actionButton size clickable sendMsg innerSvg =
    Button.view
        { actionability =
            if clickable then
                Button.Abled Button.Inactive
            else
                Button.Disabled
        , action = Pointer.onDown (always sendMsg) |> Attributes.toAttr
        , innerElement = Element.html (Icon.toHtml (0.6 * size) innerSvg)
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if clickable then
                Style.Button Style.Abled
            else
                Style.Button Style.Disabled
        , otherAttributes = []
        }
