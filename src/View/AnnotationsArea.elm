-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.AnnotationsArea
    exposing
        ( Parameters
        , view
        , viewImageOnly
        )

import Annotation.Geometry.Contour as Contour
import Annotation.Geometry.Stroke as Stroke
import Annotation.Style as Style
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Color exposing (Color)
import Control exposing (Control)
import Control.Throttle as Throttle
import Data.AnnotatedImage as AnnotatedImage
    exposing
        ( AnnotatedImage
        , Annotations(BBoxes, Outlines, Points, Polygons, Strokes)
        , BBoxDrawings
        , OutlineDrawings
        , PointDrawings
        , PolygonDrawings
        , Status(Loaded, Loading, LoadingError)
        , StrokeDrawings
        )
import Data.RawImage as RawImage exposing (RawImage)
import Element exposing (Element, el, text)
import Element.Attributes exposing (center, fill, height)
import Html.Attributes
import Image exposing (Image)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Time


type alias Parameters msg =
    { size : ( Float, Float )

    -- events
    , pointerDownMsg : ( Float, Float ) -> msg
    , pointerMoveMsg : ( Float, Float ) -> msg
    , pointerUpMsg : ( Float, Float ) -> msg
    , throttleMsg : Control msg -> msg
    }


viewImageOnly : Viewer -> RawImage -> Element Style var msg
viewImageOnly viewer { id, name, status } =
    case status of
        RawImage.Loading ->
            el Style.None [ center ] (text <| "Loading image " ++ name)

        RawImage.Loaded image ->
            Image.viewSvg [] image
                |> Viewer.viewInWithDetails [ Html.Attributes.style [ ( "height", "100%" ) ] ] viewer
                |> Element.html
                |> el Style.Viewer [ height fill ]

        RawImage.LoadingError error ->
            el Style.None [ center ] (text <| "Error with image " ++ name ++ ": " ++ error)


view : Parameters msg -> Viewer -> Int -> AnnotatedImage -> Element Style var msg
view params viewer selectedClassId { id, name, status } =
    let
        attributes =
            [ Html.Attributes.style [ ( "height", "100%" ) ]

            -- pointer capture hack to continue "globally" the event anywhere on document
            , Html.Attributes.attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
            , Pointer.onDown (.pointer >> .offsetPos >> params.pointerDownMsg)
            , Pointer.onMove (.pointer >> .offsetPos >> params.pointerMoveMsg)
                |> Html.Attributes.map (Throttle.both params.throttleMsg <| Time.second / 35)
            , Pointer.onUp (.pointer >> .offsetPos >> params.pointerUpMsg)
            ]
    in
    case status of
        Loading ->
            el Style.None [ center ] (text <| "Loading image " ++ name)

        Loaded image annotations ->
            annotationsWithImage viewer.zoom image selectedClassId annotations
                |> Viewer.viewInWithDetails attributes viewer
                |> Element.html
                |> el Style.Viewer [ height fill ]

        LoadingError error ->
            el Style.None [ center ] (text <| "Error with image " ++ name ++ ": " ++ error)


annotationsWithImage : Float -> Image -> Int -> Zipper { toolId : Int, annotations : Annotations } -> Svg msg
annotationsWithImage zoom image selectedClassId zipper =
    Zipper.getAll zipper
        |> List.map (viewAnnotations zoom selectedClassId)
        |> (::) (Image.viewSvg [] image)
        |> Svg.g []


viewAnnotations : Float -> Int -> { toolId : Int, annotations : Annotations } -> Svg msg
viewAnnotations zoom selectedClassId { annotations } =
    -- use lazy svg here
    case annotations of
        Points drawings ->
            viewPoints zoom (Color.rgba 255 255 255 0.4) selectedClassId drawings

        BBoxes drawings ->
            viewBBoxes zoom (Color.rgba 255 255 255 0.4) selectedClassId drawings

        Strokes drawings ->
            viewStrokes zoom (Color.rgba 255 255 255 0.4) selectedClassId drawings

        Outlines drawings ->
            viewOutlines zoom (Color.rgba 255 255 255 0.4) selectedClassId drawings

        Polygons drawings ->
            viewPolygons zoom (Color.rgba 255 255 255 0.4) selectedClassId drawings


viewPoints : Float -> Color -> Int -> PointDrawings -> Svg msg
viewPoints zoom fillColor selectedClassId drawings =
    let
        pointLineStyle =
            lineStyle zoom Color.black 4

        pointFillStyle =
            fillStyle fillColor

        viewOne { classId, drawing } =
            let
                highlight =
                    classId == selectedClassId
            in
            Svg.pointStyled (pointLineStyle highlight) (pointFillStyle highlight) (10 / zoom) drawing
    in
    drawings
        |> List.map viewOne
        |> Svg.g []


viewBBoxes : Float -> Color -> Int -> BBoxDrawings -> Svg msg
viewBBoxes zoom fillColor selectedClassId drawings =
    let
        bboxStrokeStyle =
            lineStyle zoom Color.black 4

        bboxFillStyle =
            fillStyle fillColor

        viewOne { classId, drawing } =
            let
                highlight =
                    classId == selectedClassId
            in
            Svg.boundingBoxStyled (bboxStrokeStyle highlight) (bboxFillStyle highlight) drawing
    in
    drawings
        |> List.map viewOne
        |> Svg.g []


viewStrokes : Float -> Color -> Int -> StrokeDrawings -> Svg msg
viewStrokes zoom color selectedClassId drawings =
    let
        viewOneBG { classId, drawing } =
            Svg.strokeStyled
                (lineStyle zoom (Color.rgba 0 0 0 0.5) 6 (classId == selectedClassId))
                drawing

        viewOneFG { classId, drawing } =
            Svg.strokeStyled
                (lineStyle zoom color 3 (classId == selectedClassId))
                drawing
    in
    List.map viewOneBG drawings
        ++ List.map viewOneFG drawings
        |> Svg.g []


viewOutlines : Float -> Color -> Int -> OutlineDrawings -> Svg msg
viewOutlines zoom fillColor selectedClassId drawings =
    let
        strokeStyle =
            lineStyle zoom Color.black 4

        outlineFillStyle =
            fillStyle fillColor

        viewOne { classId, drawing } =
            let
                highlight =
                    classId == selectedClassId
            in
            case drawing of
                AnnotatedImage.DrawingOutline stroke ->
                    Svg.strokeStyled (strokeStyle highlight) stroke

                AnnotatedImage.EndedOutline outline ->
                    Svg.outlineStyled (strokeStyle highlight) (outlineFillStyle highlight) outline
    in
    List.map viewOne drawings
        |> Svg.g []


viewPolygons : Float -> Color -> Int -> PolygonDrawings -> Svg msg
viewPolygons zoom fillColor selectedClassId drawings =
    let
        strokeStyle =
            lineStyle zoom Color.black 4

        polyFillStyle =
            fillStyle fillColor

        pointStyled highlight =
            Svg.pointStyled (strokeStyle highlight) (polyFillStyle highlight) (10 / zoom)

        viewOne { classId, drawing } =
            let
                highlight =
                    classId == selectedClassId
            in
            case drawing of
                AnnotatedImage.PolygonStartedAt _ stroke ->
                    Svg.strokeStyled (strokeStyle highlight) stroke
                        :: List.map (pointStyled highlight) (Stroke.points stroke)
                        |> Svg.g []

                AnnotatedImage.EndedPolygon polygon ->
                    Svg.contourStyled (strokeStyle highlight) (polyFillStyle highlight) polygon
                        :: List.map (pointStyled highlight) (Contour.points polygon)
                        |> Svg.g []
    in
    List.map viewOne drawings
        |> Svg.g []


lineStyle : Float -> Color -> Float -> Bool -> Style.Line
lineStyle zoom color size highlight =
    if highlight then
        Style.Stroke (size / zoom) color
    else
        Style.Stroke (size / zoom) (moreTransparent color)


fillStyle : Color -> Bool -> Style.Fill
fillStyle color highlight =
    if highlight then
        Style.Fill (moreOpaque color)
    else
        Style.Fill color


moreOpaque : Color -> Color
moreOpaque color =
    let
        rgba =
            Color.toRgb color

        newAlpha =
            0.25 * (3 + rgba.alpha)
    in
    Color.rgba rgba.red rgba.green rgba.blue newAlpha


moreTransparent : Color -> Color
moreTransparent color =
    let
        rgba =
            Color.toRgb color

        newAlpha =
            0.25 * rgba.alpha
    in
    Color.rgba rgba.red rgba.green rgba.blue newAlpha
