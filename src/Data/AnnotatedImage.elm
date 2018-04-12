-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.AnnotatedImage
    exposing
        ( AnnotatedImage
        , Annotations(..)
        , BBoxDrawings
        , OneOutlineDrawing(..)
        , OnePolygonDrawing(..)
        , OutlineDrawings
        , PointDrawings
        , PolygonDrawings
        , Status(..)
        , StrokeDrawings
        , annotationsFromTools
        , fromRaw
        , hasAnnotations
        , removeLatestAnnotation
        , resetWithTools
        , selectTool
        , updateWithPointer
        )

import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Stroke as Stroke
import Annotation.Geometry.Types exposing (BoundingBox, Contour, Outline, Point, Stroke)
import Data.Pointer as Pointer
import Data.RawImage as RawImage exposing (RawImage)
import Data.Tool as Tool exposing (Tool)
import Image exposing (Image)
import Packages.Zipper as Zipper exposing (Zipper)


-- TYPES #############################################################


type alias AnnotatedImage =
    { id : Int
    , name : String
    , status : Status
    }


type Status
    = Loading
    | Loaded Image (Zipper { toolId : Int, annotations : Annotations })
    | LoadingError String


type Annotations
    = Points PointDrawings
    | BBoxes BBoxDrawings
    | Strokes StrokeDrawings
    | Outlines OutlineDrawings
    | Polygons PolygonDrawings


type alias PointDrawings =
    List { classId : Int, drawing : Point }


type alias BBoxDrawings =
    List { classId : Int, drawing : BoundingBox }


type alias StrokeDrawings =
    List { classId : Int, drawing : Stroke }


type alias OutlineDrawings =
    List { classId : Int, drawing : OneOutlineDrawing }


type alias PolygonDrawings =
    List { classId : Int, drawing : OnePolygonDrawing }


type OneOutlineDrawing
    = DrawingOutline Stroke
    | EndedOutline Outline


type OnePolygonDrawing
    = PolygonStartedAt ( Float, Float ) Stroke
    | EndedPolygon Contour



-- FUNCTIONS #########################################################


hasAnnotations : AnnotatedImage -> Bool
hasAnnotations annotatedImage =
    case annotatedImage.status of
        Loaded _ annotations ->
            case .annotations (Zipper.getC annotations) of
                Points (_ :: _) ->
                    True

                BBoxes (_ :: _) ->
                    True

                Strokes (_ :: _) ->
                    True

                Outlines (_ :: _) ->
                    True

                Polygons (_ :: _) ->
                    True

                _ ->
                    False

        _ ->
            False


selectTool : Int -> AnnotatedImage -> AnnotatedImage
selectTool toolId annotatedImage =
    case annotatedImage.status of
        Loaded img annotations ->
            { annotatedImage | status = Loaded img (Zipper.goTo .toolId toolId annotations) }

        _ ->
            annotatedImage


focusUpdate : (Annotations -> Annotations) -> AnnotatedImage -> AnnotatedImage
focusUpdate f ({ id, name, status } as annotatedImage) =
    case status of
        Loaded img zipper ->
            { annotatedImage
                | status =
                    Loaded img (Zipper.updateC (\a -> { a | annotations = f a.annotations }) zipper)
            }

        _ ->
            annotatedImage


removeLatestAnnotation : AnnotatedImage -> AnnotatedImage
removeLatestAnnotation =
    focusUpdate removeLatest


removeLatest : Annotations -> Annotations
removeLatest annotations =
    case annotations of
        Points drawings ->
            Points (List.drop 1 drawings)

        BBoxes drawings ->
            BBoxes (List.drop 1 drawings)

        Strokes drawings ->
            Strokes (List.drop 1 drawings)

        Outlines drawings ->
            Outlines (List.drop 1 drawings)

        Polygons drawings ->
            Polygons (List.drop 1 drawings)



-- Pointer stuff


addAnnotationsIndicator : (List a -> Annotations) -> ( List a, b, c ) -> ( Annotations, b, Bool, c )
addAnnotationsIndicator type_ ( list, dragState, hasChanged ) =
    ( type_ list, dragState, not (List.isEmpty list), hasChanged )


updateWithPointer : Float -> Int -> Pointer.Msg -> Pointer.DragState -> AnnotatedImage -> ( AnnotatedImage, Pointer.DragState, Bool, Bool )
updateWithPointer zoom selectedClassId pointerMsg dragState ({ id, name, status } as annotatedImage) =
    case status of
        Loaded img zipper ->
            let
                { toolId, annotations } =
                    Zipper.getC zipper

                ( newAnnotations, newDragState, hasAnnotations, hasChanged ) =
                    case annotations of
                        Points drawings ->
                            updatePoints selectedClassId pointerMsg dragState drawings
                                |> addAnnotationsIndicator Points

                        BBoxes drawings ->
                            updateBBoxes selectedClassId pointerMsg dragState drawings
                                |> addAnnotationsIndicator BBoxes

                        Strokes drawings ->
                            updateStrokes selectedClassId pointerMsg dragState drawings
                                |> addAnnotationsIndicator Strokes

                        Outlines drawings ->
                            updateOutlines selectedClassId pointerMsg dragState drawings
                                |> addAnnotationsIndicator Outlines

                        Polygons drawings ->
                            updatePolygons zoom selectedClassId pointerMsg dragState drawings
                                |> addAnnotationsIndicator Polygons

                newStatus =
                    Loaded img (Zipper.setC { toolId = toolId, annotations = newAnnotations } zipper)
            in
            ( { annotatedImage | status = newStatus }, newDragState, hasAnnotations, hasChanged )

        _ ->
            ( annotatedImage, dragState, False, False )


updatePoints : Int -> Pointer.Msg -> Pointer.DragState -> PointDrawings -> ( PointDrawings, Pointer.DragState, Bool )
updatePoints selectedClassId pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( Pointer.DownAt pos, Pointer.NoDrag, _ ) ->
            ( { classId = selectedClassId, drawing = Point.fromCoordinates pos } :: drawings
            , Pointer.DraggingFrom pos
            , True
            )

        ( Pointer.MoveAt pos, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            ( { d | drawing = Point.fromCoordinates pos } :: otherDrawings
            , dragState
            , True
            )

        ( Pointer.UpAt pos, _, _ ) ->
            ( drawings, Pointer.NoDrag, True )

        _ ->
            ( drawings, dragState, False )


updateBBoxes : Int -> Pointer.Msg -> Pointer.DragState -> BBoxDrawings -> ( BBoxDrawings, Pointer.DragState, Bool )
updateBBoxes selectedClassId pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( Pointer.DownAt pos, Pointer.NoDrag, _ ) ->
            let
                point =
                    Point.fromCoordinates pos

                bbox =
                    BoundingBox.fromPair ( point, point )
            in
            ( { classId = selectedClassId, drawing = bbox } :: drawings
            , Pointer.DraggingFrom pos
            , True
            )

        ( Pointer.MoveAt pos, Pointer.DraggingFrom corner, d :: otherDrawings ) ->
            let
                bbox =
                    BoundingBox.fromPair
                        ( Point.fromCoordinates corner
                        , Point.fromCoordinates pos
                        )
            in
            ( { d | drawing = bbox } :: otherDrawings
            , dragState
            , True
            )

        ( Pointer.UpAt ( x1, y1 ), Pointer.DraggingFrom ( x2, y2 ), d :: otherDrawings ) ->
            if x1 == x2 || y1 == y2 then
                ( otherDrawings, Pointer.NoDrag, True )
            else
                ( drawings, Pointer.NoDrag, True )

        _ ->
            ( drawings, dragState, False )


updateStrokes : Int -> Pointer.Msg -> Pointer.DragState -> StrokeDrawings -> ( StrokeDrawings, Pointer.DragState, Bool )
updateStrokes selectedClassId pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( Pointer.DownAt pos, Pointer.NoDrag, _ ) ->
            let
                stroke =
                    Stroke.fromPoints [ Point.fromCoordinates pos ]
            in
            ( { classId = selectedClassId, drawing = stroke } :: drawings
            , Pointer.DraggingFrom pos
            , True
            )

        ( Pointer.MoveAt pos, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            let
                stroke =
                    Stroke.addPoint (Point.fromCoordinates pos) d.drawing
            in
            ( { d | drawing = stroke } :: otherDrawings
            , dragState
            , True
            )

        ( Pointer.UpAt _, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            ( drawings, Pointer.NoDrag, True )

        _ ->
            ( drawings, dragState, False )


updateOutlines : Int -> Pointer.Msg -> Pointer.DragState -> OutlineDrawings -> ( OutlineDrawings, Pointer.DragState, Bool )
updateOutlines selectedClassId pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( Pointer.DownAt pos, Pointer.NoDrag, _ ) ->
            let
                outline =
                    DrawingOutline (Stroke.fromPoints [ Point.fromCoordinates pos ])
            in
            ( { classId = selectedClassId, drawing = outline } :: drawings
            , Pointer.DraggingFrom pos
            , True
            )

        ( Pointer.MoveAt pos, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            case d.drawing of
                DrawingOutline stroke ->
                    let
                        outline =
                            DrawingOutline (Stroke.addPoint (Point.fromCoordinates pos) stroke)
                    in
                    ( { d | drawing = outline } :: otherDrawings
                    , dragState
                    , True
                    )

                _ ->
                    ( drawings, dragState, False )

        ( Pointer.UpAt _, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            case d.drawing of
                DrawingOutline stroke ->
                    ( { d | drawing = EndedOutline (Stroke.close stroke) } :: otherDrawings
                    , Pointer.NoDrag
                    , True
                    )

                _ ->
                    ( drawings, dragState, False )

        _ ->
            ( drawings, dragState, False )


updatePolygons : Float -> Int -> Pointer.Msg -> Pointer.DragState -> PolygonDrawings -> ( PolygonDrawings, Pointer.DragState, Bool )
updatePolygons zoom selectedClassId pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( Pointer.DownAt pos, Pointer.NoDrag, [] ) ->
            let
                point =
                    Point.fromCoordinates pos

                polygon =
                    PolygonStartedAt pos (Stroke.fromPoints [ point ])
            in
            ( { classId = selectedClassId, drawing = polygon } :: []
            , Pointer.DraggingFrom pos
            , True
            )

        ( Pointer.DownAt pos, Pointer.NoDrag, d :: otherDrawings ) ->
            case d.drawing of
                EndedPolygon polygon ->
                    let
                        point =
                            Point.fromCoordinates pos

                        polygon =
                            PolygonStartedAt pos (Stroke.fromPoints [ point ])
                    in
                    ( { classId = selectedClassId, drawing = polygon } :: drawings
                    , Pointer.DraggingFrom pos
                    , True
                    )

                PolygonStartedAt startPos stroke ->
                    let
                        point =
                            Point.fromCoordinates pos

                        polygon =
                            PolygonStartedAt startPos (Stroke.addPoint point stroke)
                    in
                    ( { d | drawing = polygon } :: otherDrawings
                    , Pointer.DraggingFrom pos
                    , True
                    )

        ( Pointer.MoveAt pos, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            case d.drawing of
                PolygonStartedAt startPos stroke ->
                    let
                        point =
                            Point.fromCoordinates pos

                        ( newStartPos, newStroke ) =
                            case Stroke.points stroke of
                                _ :: [] ->
                                    ( pos, Stroke.fromPoints [ point ] )

                                _ :: rest ->
                                    ( startPos, Stroke.fromPoints (point :: rest) )

                                [] ->
                                    ( startPos, stroke )

                        polygon =
                            PolygonStartedAt newStartPos newStroke
                    in
                    ( { d | drawing = polygon } :: otherDrawings
                    , dragState
                    , True
                    )

                _ ->
                    ( drawings, dragState, False )

        ( Pointer.UpAt pos, Pointer.DraggingFrom _, d :: otherDrawings ) ->
            case d.drawing of
                PolygonStartedAt startPos stroke ->
                    case Stroke.points stroke of
                        _ :: [] ->
                            ( drawings, Pointer.NoDrag, True )

                        point :: rest ->
                            -- Should use model.viewer.zoom here
                            if distance pos startPos > (30 / zoom) then
                                ( drawings, Pointer.NoDrag, True )
                            else
                                let
                                    polygon =
                                        EndedPolygon (Stroke.close (Stroke.fromPoints rest))
                                in
                                ( { d | drawing = polygon } :: otherDrawings
                                , Pointer.NoDrag
                                , True
                                )

                        [] ->
                            ( drawings, Pointer.NoDrag, True )

                _ ->
                    ( drawings, dragState, False )

        _ ->
            ( drawings, dragState, False )


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)



-- Conversion from raw image


fromRaw : Zipper Tool -> RawImage -> AnnotatedImage
fromRaw tools { id, name, status } =
    let
        annotatedStatus =
            case status of
                RawImage.Loading ->
                    Loading

                RawImage.LoadingError error ->
                    LoadingError error

                RawImage.Loaded image ->
                    Loaded image (annotationsFromTools tools)
    in
    { id = id, name = name, status = annotatedStatus }


resetWithTools : Zipper Tool -> AnnotatedImage -> AnnotatedImage
resetWithTools tools image =
    case image.status of
        Loaded img _ ->
            { image | status = Loaded img (annotationsFromTools tools) }

        _ ->
            image


annotationsFromTools : Zipper Tool -> Zipper { toolId : Int, annotations : Annotations }
annotationsFromTools tools =
    let
        filteredTools =
            Zipper.getAll tools
                |> List.filter (\tool -> tool.type_ /= Tool.Move)

        fromTool tool =
            let
                emptyAnnotations =
                    case tool.type_ of
                        Tool.Point ->
                            Points []

                        Tool.BBox ->
                            BBoxes []

                        Tool.Stroke ->
                            Strokes []

                        Tool.Outline ->
                            Outlines []

                        Tool.Polygon ->
                            Polygons []

                        _ ->
                            Debug.crash "A tool should only be of some kinds"
            in
            { toolId = tool.id, annotations = emptyAnnotations }
    in
    case filteredTools of
        [] ->
            Debug.crash "Config should always provide at least one annotation tool"

        tool :: otherTools ->
            Zipper.init [] (fromTool tool) (List.map fromTool otherTools)
