module View.ImageAnnotations exposing (..)

import Annotation exposing (PointerMsg(..))
import Annotation.Geometry.Stroke as Stroke
import Annotation.Style as Style
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Color exposing (Color)
import Control.Throttle as Throttle
import Element exposing (Element)
import Element.Attributes
import Html.Attributes
import Image exposing (Image)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Svg.Lazy
import Time
import Tool exposing (Tool)
import Types exposing (..)


imageViewer : Viewer -> Int -> ImageData -> Element Style variation Msg
imageViewer viewer selectedClass imageData =
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

        ( maybeImage, toolsData ) =
            case imageData of
                Loaded _ _ image toolsData ->
                    ( Just image, toolsData )

                _ ->
                    ( Nothing, Zipper.init [] (Tool.Data 0 Tool.Move 0) [] )
    in
    annotationsWithImage viewer.zoom maybeImage selectedClass toolsData
        |> Viewer.viewInWithDetails attributes viewer
        |> Element.html
        |> Element.el Style.Viewer
            [ Element.Attributes.height Element.Attributes.fill ]


annotationsWithImage : Float -> Maybe Image -> Int -> Zipper Tool.Data -> Svg msg
annotationsWithImage zoom maybeImage selectedClass toolsData =
    Zipper.getAll toolsData
        |> List.drop 1
        |> List.map (viewAnnotationData True zoom selectedClass)
        |> (::) (viewImage maybeImage)
        |> Svg.g []


viewAnnotationData : Bool -> Float -> Int -> Tool.Data -> Svg msg
viewAnnotationData highlight zoom selectedClass toolData =
    case toolData.tool of
        Tool.Move ->
            Svg.text "Move tool"

        Tool.Annotation (Annotation.Point pointDrawings) ->
            viewPoint zoom Color.blue pointDrawings

        Tool.Annotation (Annotation.BBox bboxDrawings) ->
            viewBBox zoom (Color.rgba 255 255 255 0.4) selectedClass bboxDrawings

        Tool.Annotation (Annotation.Stroke drawings) ->
            viewStroke zoom Color.purple drawings

        Tool.Annotation (Annotation.Outline drawings) ->
            viewOutline zoom Color.purple drawings

        Tool.Annotation (Annotation.Polygon drawings) ->
            viewPolygon zoom Color.purple drawings


viewImage : Maybe Image -> Svg msg
viewImage maybeImage =
    case maybeImage of
        Nothing ->
            Svg.text "No background image"

        Just image ->
            Image.viewSvg [] image


viewPoint : Float -> Color -> Annotation.PointDrawings -> Svg msg
viewPoint zoom color pointDrawings =
    pointDrawings
        |> List.map (Svg.pointStyled (Style.Disk (10 / zoom) color))
        |> Svg.g []


viewStroke : Float -> Color -> Annotation.StrokeDrawings -> Svg msg
viewStroke zoom color strokeDrawings =
    strokeDrawings
        |> List.map (Svg.strokeStyled <| Style.Stroke (2 / zoom) color)
        |> Svg.g []


twiceOpaque : Color -> Color
twiceOpaque color =
    let
        rgba =
            Color.toRgb color

        newAlpha =
            0.5 * (1 + rgba.alpha)
    in
    Color.rgba rgba.red rgba.green rgba.blue newAlpha


viewBBox : Float -> Color -> Int -> Annotation.BBoxDrawings -> Svg msg
viewBBox zoom color selectedClass bboxDrawings =
    let
        viewOneBBox ( id, bbox ) =
            let
                highlight =
                    id == selectedClass

                strokeStyle =
                    if highlight then
                        Style.Stroke (4 / zoom) Color.black
                    else
                        Style.NoLine

                fillStyle =
                    if highlight then
                        Style.Fill (twiceOpaque color)
                    else
                        Style.Fill color
            in
            Svg.boundingBoxStyled strokeStyle fillStyle bbox
    in
    bboxDrawings
        |> List.map viewOneBBox
        |> Svg.g []


viewOutline : Float -> Color -> Annotation.OutlineDrawings -> Svg msg
viewOutline zoom color outlineDrawings =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) color

        viewOne one =
            case one of
                Annotation.DrawingOutline stroke ->
                    Svg.strokeStyled strokeStyle stroke

                Annotation.EndedOutline outline ->
                    Svg.outlineStyled strokeStyle Style.fillDefault outline
    in
    List.map viewOne outlineDrawings
        |> Svg.g []


viewPolygon : Float -> Color -> Annotation.PolygonDrawings -> Svg msg
viewPolygon zoom color polygonDrawings =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) Color.red

        viewOne one =
            case one of
                Annotation.EndedPolygon contour ->
                    Svg.contourStyled strokeStyle Style.fillDefault contour

                Annotation.PolygonStartedAt _ stroke ->
                    Stroke.points stroke
                        |> List.map (Svg.pointStyled <| Style.Disk (10 / zoom) Color.orange)
                        |> (::) (Svg.strokeStyled strokeStyle stroke)
                        |> Svg.g []
    in
    List.map viewOne polygonDrawings
        |> Svg.g []
