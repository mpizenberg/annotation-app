module View.ImageAnnotations exposing (..)

import Annotation
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
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Time
import Tool exposing (Tool)
import Types exposing (..)


imageViewer : Viewer -> Maybe Image -> List Tool.Data -> Element Style variation Msg
imageViewer viewer maybeImage toolsData =
    let
        attributes =
            [ Html.Attributes.style [ ( "height", "100%" ) ]
            , Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt >> PointerMsg)
            , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt >> PointerMsg)
                |> Html.Attributes.map (Throttle.both MoveThrottle <| Time.second / 35)
            , Pointer.onUp (.pointer >> .offsetPos >> PointerUpAt >> PointerMsg)
            ]
    in
    toolsData
        |> List.map (viewAnnotationData viewer)
        |> Svg.g []
        |> Viewer.viewInWithDetails attributes viewer
        |> Element.html
        |> Element.el Style.Viewer
            [ Element.Attributes.height Element.Attributes.fill ]


viewAnnotationData : Viewer -> Tool.Data -> Svg msg
viewAnnotationData viewer toolData =
    case toolData.tool of
        Tool.Move ->
            Svg.text "Move tool"

        Tool.Annotation (Annotation.Point pointDrawings) ->
            viewPoint viewer.zoom Color.blue pointDrawings

        Tool.Annotation (Annotation.BBox bboxDrawings) ->
            viewBBox viewer.zoom Color.purple bboxDrawings


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


viewBBox : Float -> Color -> Annotation.BBoxDrawings -> Svg msg
viewBBox zoom color bboxDrawings =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) color
    in
    bboxDrawings
        |> List.map (Svg.boundingBoxStyled strokeStyle Style.fillDefault)
        |> Svg.g []



-- TODO Below are functions to modify


viewOutline : Float -> Annotation.OutlineDrawing -> Svg msg
viewOutline zoom outlineDrawing =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) Color.red
    in
    case outlineDrawing of
        Annotation.NoOutline ->
            Svg.text "No outline"

        Annotation.DrawingOutline stroke ->
            Svg.strokeStyled strokeStyle stroke

        Annotation.EndedOutline outline ->
            Svg.outlineStyled strokeStyle Style.fillDefault outline


viewContour : Float -> Annotation.ContourDrawing -> Svg msg
viewContour zoom contourDrawing =
    let
        strokeStyle =
            Style.Stroke (2 / zoom) Color.red
    in
    case contourDrawing of
        Annotation.NoContour ->
            Svg.text "No contour"

        Annotation.Ended contour ->
            Svg.contourStyled strokeStyle Style.fillDefault contour

        Annotation.DrawingStartedAt _ stroke ->
            Stroke.points stroke
                |> List.map (Svg.pointStyled <| Style.Disk (10 / zoom) Color.orange)
                |> (::) (Svg.strokeStyled strokeStyle stroke)
                |> Svg.g []
