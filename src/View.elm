module View exposing (view)

import Annotation.Geometry.Stroke as Stroke
import Annotation.Geometry.Types exposing (..)
import Annotation.Style as Style
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Button exposing (Button)
import Color
import Control.Throttle as Throttle
import Device exposing (Device)
import Element exposing (Element, below, el, empty, node, span)
import Element.Attributes as Attributes exposing (Length, alignRight, center, fill, px, verticalCenter)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy exposing (lazy2)
import Icons
import Image exposing (Image)
import Json.Decode as Decode
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


responsiveLayout : Model -> Element Style variation Msg
responsiveLayout model =
    Element.column Style.None
        [ Attributes.height fill ]
        [ deviceActionBar model.device model.tool model.currentDropdownTool model.toolDropdownOpen model.layout.actionBarSize
        , imageViewer model
        ]


deviceActionBar : Device -> Tool -> Tool -> Bool -> ( Float, Float ) -> Element Style variation Msg
deviceActionBar device currentTool currentDropdownTool toolDropdownOpen ( width, height ) =
    let
        filler =
            el Style.None [ Attributes.width fill, Attributes.height (px height) ] empty

        toolButtons =
            case device.kind of
                Device.Phone ->
                    [ toolButton height currentTool Tool.Move
                    , toolDropdown height currentTool currentDropdownTool toolDropdownOpen
                    , actionButton height True ToggleToolDropdown Icons.moreVertical
                    ]

                _ ->
                    (Tool.Move :: Tool.allAnnotationTools)
                        |> List.map (toolButton height currentTool)

        actionButtons =
            [ actionButton height False NoOp Icons.rotateCcw
            , actionButton height True NoOp Icons.trash2
            , loadFileInput height Icons.image
            ]

        zoomActions =
            [ actionButton height True (ZoomMsg ZoomIn) Icons.zoomIn
            , actionButton height True (ZoomMsg ZoomOut) Icons.zoomOut
            , actionButton height True (ZoomMsg ZoomFit) Icons.zoomFit
            ]
    in
    case ( device.kind, device.orientation ) of
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


actionButton : Float -> Bool -> Msg -> List (Svg Msg) -> Element Style v Msg
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
        , outerStyle = Style.Button (not clickable)
        , otherAttributes = [ Attributes.attribute "elm-pep" "true" ]
        }


loadFileInput : Float -> List (Svg Msg) -> Element Style variation Msg
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
                , outerStyle = Style.Button False
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


toolDropdown : Float -> Tool -> Tool -> Bool -> Element Style variation Msg
toolDropdown size currentTool currentDropdownTool toolDropdownOpen =
    let
        downTools =
            Tool.allAnnotationTools
                |> List.filter ((/=) currentDropdownTool)
                |> List.map (toolButton size currentTool)
                |> Element.column Style.None []
                |> List.singleton
    in
    el Style.None [] (toolButton size currentTool currentDropdownTool)
        |> below
            (if toolDropdownOpen then
                downTools
             else
                []
            )


toolButton : Float -> Tool -> Tool -> Element Style variation Msg
toolButton size currentTool tool =
    Button.view
        { actionability =
            Button.Abled
                (if tool == currentTool then
                    Button.Active
                 else
                    Button.Inactive
                )
        , action = Pointer.onDown (always <| SelectTool tool) |> Attributes.toAttr
        , innerElement = Tool.svgElement (0.6 * size) tool
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if tool == currentTool then
                Style.CurrentTool
            else
                Style.Button False
        , otherAttributes = [ Attributes.attribute "elm-pep" "true" ]
        }


imageViewer : Model -> Element Style variation Msg
imageViewer model =
    let
        attributes =
            [ Html.Attributes.style [ ( "height", "100%" ) ]
            , Html.Attributes.attribute "elm-pep" "true"
            , Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt >> PointerMsg)
            , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt >> PointerMsg)
                |> Html.Attributes.map (Throttle.both MoveThrottle <| Time.second / 35)
            , Pointer.onUp (.pointer >> .offsetPos >> PointerUpAt >> PointerMsg)
            ]
    in
    []
        |> (::) (viewPoint model.viewer.zoom model.point)
        |> (::) (viewStroke model.viewer.zoom model.stroke)
        |> (::) (viewBBox model.viewer.zoom model.bbox)
        |> (::) (viewContour model.viewer.zoom model.contour)
        |> (::) (viewOutline model.viewer.zoom model.outline)
        |> (::) (viewImage model.image)
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


viewOutline : Float -> OutlineDrawing -> Svg msg
viewOutline zoom outlineDrawing =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) Color.red
    in
    case outlineDrawing of
        NoOutline ->
            Svg.text "No outline"

        DrawingOutline stroke ->
            Svg.strokeStyled strokeStyle stroke

        EndedOutline outline ->
            Svg.outlineStyled strokeStyle Style.fillDefault outline


viewContour : Float -> ContourDrawing -> Svg msg
viewContour zoom contourDrawing =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) Color.red
    in
    case contourDrawing of
        NoContour ->
            Svg.text "No contour"

        Ended contour ->
            Svg.contourStyled strokeStyle Style.fillDefault contour

        DrawingStartedAt _ stroke ->
            Stroke.points stroke
                |> List.map (Svg.pointStyled <| Style.Disk (10 / zoom) Color.orange)
                |> (::) (Svg.strokeStyled strokeStyle stroke)
                |> Svg.g []
