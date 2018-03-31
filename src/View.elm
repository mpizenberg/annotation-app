module View exposing (view)

import Annotation.Geometry.Types exposing (..)
import Annotation.Style as Style
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Color
import Control.Throttle as Throttle
import Element exposing (Element, below, el, empty, node, span)
import Element.Attributes as Attributes exposing (Length, alignRight, center, fill, px, verticalCenter)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy exposing (lazy2)
import Icons
import Image exposing (Image)
import Json.Decode as Decode
import Packages.Button as Button exposing (Button)
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Time
import Tool exposing (Tool)
import Types exposing (..)


view : Model -> Html Msg
view model =
    responsiveLayout model
        |> Element.layout Style.sheet


responsiveLayout : Model -> Element Style Style.ColorVariations Msg
responsiveLayout model =
    let
        hasAnnotation =
            -- (model.point /= Nothing)
            --     || (model.stroke /= Nothing)
            --     || (model.bbox /= Nothing)
            --     || (model.outline /= NoOutline)
            --     || (model.contour /= NoContour)
            False

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
        [ deviceActionBar actionBarParameters
        , imageViewer model
        ]


type alias ActionBarParameters =
    { device : Device
    , size : ( Float, Float )
    , canClearAnnotations : Bool
    , hasImage : Bool
    , toolsData : Zipper Tool.Data
    }


deviceActionBar : ActionBarParameters -> Element Style Style.ColorVariations Msg
deviceActionBar param =
    let
        ( width, height ) =
            param.size

        filler =
            el Style.None [ Attributes.width fill, Attributes.height (px height) ] empty

        selectedToolButton : Tool.Data -> Element Style Style.ColorVariations Msg
        selectedToolButton =
            toolButton height True

        unselectedToolButton : Tool.Data -> Element Style Style.ColorVariations Msg
        unselectedToolButton =
            toolButton height False

        toolButtons =
            List.concat
                [ Zipper.getL param.toolsData
                    |> List.map unselectedToolButton
                , [ selectedToolButton (Zipper.getC param.toolsData) ]
                , Zipper.getR param.toolsData
                    |> List.map unselectedToolButton
                ]

        actionButtons =
            [ actionButton height param.canClearAnnotations ClearAnnotations Icons.trash2
            , loadFileInput height Icons.image
            ]

        zoomActions =
            [ actionButton height True (ZoomMsg ZoomIn) Icons.zoomIn
            , actionButton height True (ZoomMsg ZoomOut) Icons.zoomOut
            , actionButton height param.hasImage (ZoomMsg ZoomFit) Icons.zoomFit
            ]
    in
    case ( param.device.kind, param.device.orientation ) of
        ( Device.Phone, Device.Portrait ) ->
            (toolButtons ++ filler :: actionButtons)
                |> Element.row Style.None []
                |> below [ el Style.None [ alignRight ] (Element.row Style.None [] zoomActions) ]

        ( Device.Tablet, Device.Portrait ) ->
            (toolButtons ++ filler :: actionButtons)
                |> Element.row Style.None []
                |> below [ el Style.None [ alignRight ] (Element.row Style.None [] zoomActions) ]

        _ ->
            (toolButtons ++ filler :: actionButtons ++ filler :: zoomActions)
                |> Element.row Style.None []


actionButton : Float -> Bool -> Msg -> List (Svg Msg) -> Element Style Style.ColorVariations Msg
actionButton size clickable sendMsg innerSvg =
    Button.view
        { actionability =
            if clickable then
                Button.Abled Button.Inactive
            else
                Button.Disabled
        , action = Pointer.onDown (always sendMsg) |> Attributes.toAttr
        , innerElement = Element.html (lazy2 Icons.sized (0.6 * size) innerSvg)
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if clickable then
                Style.Button Style.Abled
            else
                Style.Button Style.Disabled
        , otherAttributes = []
        }


loadFileInput : Float -> List (Svg Msg) -> Element Style Style.ColorVariations Msg
loadFileInput size innerSvg =
    let
        invisibleInput =
            Html.input
                [ Html.Attributes.id "file-input"
                , Html.Attributes.type_ "file"
                , Html.Attributes.style [ ( "display", "none" ) ]
                , loadFileEvent
                ]
                []

        labelButton =
            Button.view
                { actionability = Button.Abled Button.Inactive
                , action = Html.Attributes.for "file-input" |> Attributes.toAttr
                , innerElement = Element.html (lazy2 Icons.sized (0.6 * size) innerSvg)
                , innerStyle = Style.None
                , size = ( size, size )
                , outerStyle = Style.Button Style.Abled
                , otherAttributes = []
                }
    in
    Element.row Style.None [] [ Element.html invisibleInput, node "label" labelButton ]


loadFileEvent : Html.Attribute Msg
loadFileEvent =
    Decode.at [ "target", "files", "0" ] Decode.value
        |> Decode.map LoadImageFile
        |> Html.Events.onWithOptions "change" stopAndPrevent


stopAndPrevent : Html.Events.Options
stopAndPrevent =
    { stopPropagation = True
    , preventDefault = True
    }



-- toolDropdown : Float -> Tool -> Tool -> Bool -> Element Style Style.ColorVariations Msg
-- toolDropdown size currentTool currentDropdownTool toolDropdownOpen =
--     let
--         downTools =
--             Tool.allAnnotationTools
--                 |> List.filter ((/=) currentDropdownTool)
--                 |> List.map (toolButton size currentTool)
--                 |> Element.column Style.None []
--                 |> List.singleton
--     in
--     el Style.None [] (toolButton size currentTool currentDropdownTool)
--         |> below
--             (if toolDropdownOpen then
--                 downTools
--              else
--                 []
--             )
--
--
-- toolButtonBis : Float -> Bool -> ToolBis -> Int -> Element Style Style.ColorVariations Msg
-- toolButtonBis size isSelected tool toolId =
--     Button.view
--         { actionability =
--             if isSelected then
--                 Button.Abled Button.Active
--             else
--                 Button.Abled Button.Inactive
--         , action = Pointer.onDown (always <| SelectTool ( Just toolId, tool )) |> Attributes.toAttr
--         , innerElement = Tool.toolSvg (0.6 * size) tool
--         , innerStyle = Style.None
--         , size = ( size, size )
--         , outerStyle =
--             if isSelected then
--                 Style.Button Style.Selected
--             else
--                 Style.Button Style.Abled
--         , otherAttributes = []
--         }


toolButton : Float -> Bool -> Tool.Data -> Element Style Style.ColorVariations Msg
toolButton size isSelected toolData =
    Button.view
        { actionability =
            if isSelected then
                Button.Abled Button.Active
            else
                Button.Abled Button.Inactive
        , action = Pointer.onDown (always <| SelectTool toolData.id) |> Attributes.toAttr
        , innerElement = Tool.svgElement (0.6 * size) toolData
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if isSelected then
                Style.Button Style.Selected
            else
                Style.Button Style.Abled
        , otherAttributes = []
        }


imageViewer : Model -> Element Style variation Msg
imageViewer model =
    let
        attributes =
            [ Html.Attributes.style [ ( "height", "100%" ) ]

            -- pointer capture hack to continue "globally" the event anywhere on document
            , Html.Attributes.attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
            , Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt >> PointerMsg)
            , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt >> PointerMsg)
                |> Html.Attributes.map (Throttle.both MoveThrottle <| Time.second / 35)
            , Pointer.onUp (.pointer >> .offsetPos >> PointerUpAt >> PointerMsg)
            ]
    in
    []
        -- |> (::) (viewPoint model.viewer.zoom model.point)
        -- |> (::) (viewStroke model.viewer.zoom model.stroke)
        -- |> (::) (viewBBox model.viewer.zoom model.bbox)
        -- |> (::) (viewContour model.viewer.zoom model.contour)
        -- |> (::) (viewOutline model.viewer.zoom model.outline)
        -- |> (::) (viewImage model.image)
        |> Svg.g []
        |> Viewer.viewInWithDetails attributes model.viewer
        |> Element.html
        |> el Style.Viewer [ Attributes.height fill ]


viewImage : Maybe Image -> Svg msg
viewImage maybeImage =
    case maybeImage of
        Nothing ->
            Svg.text "No background image"

        Just image ->
            Image.viewSvg [] image


viewPoint : Float -> Maybe Point -> Svg msg
viewPoint zoom maybePoint =
    maybePoint
        |> Maybe.map (Svg.pointStyled (Style.Disk (10 / zoom) Color.blue))
        |> Maybe.withDefault (Svg.text "No point there")


viewStroke : Float -> Maybe Stroke -> Svg msg
viewStroke zoom maybeStroke =
    maybeStroke
        |> Maybe.map (Svg.strokeStyled <| Style.Stroke (2 / zoom) Color.purple)
        |> Maybe.withDefault (Svg.text "No stroke")


viewBBox : Float -> Maybe BoundingBox -> Svg msg
viewBBox zoom maybeBBox =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) Color.red
    in
    maybeBBox
        |> Maybe.map (Svg.boundingBoxStyled strokeStyle Style.fillDefault)
        |> Maybe.withDefault (Svg.text "No bounding box")



-- viewOutline : Float -> OutlineDrawing -> Svg msg
-- viewOutline zoom outlineDrawing =
--     let
--         strokeStyle =
--             Style.Stroke (2 / zoom) Color.red
--     in
--     case outlineDrawing of
--         NoOutline ->
--             Svg.text "No outline"
--
--         DrawingOutline stroke ->
--             Svg.strokeStyled strokeStyle stroke
--
--         EndedOutline outline ->
--             Svg.outlineStyled strokeStyle Style.fillDefault outline
--
--
-- viewContour : Float -> ContourDrawing -> Svg msg
-- viewContour zoom contourDrawing =
--     let
--         strokeStyle =
--             Style.Stroke (2 / zoom) Color.red
--     in
--     case contourDrawing of
--         NoContour ->
--             Svg.text "No contour"
--
--         Ended contour ->
--             Svg.contourStyled strokeStyle Style.fillDefault contour
--
--         DrawingStartedAt _ stroke ->
--             Stroke.points stroke
--                 |> List.map (Svg.pointStyled <| Style.Disk (10 / zoom) Color.orange)
--                 |> (::) (Svg.strokeStyled strokeStyle stroke)
--                 |> Svg.g []
