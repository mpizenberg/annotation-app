-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Annotation exposing
    ( Annotation(..)
    , encode
    )

import Annotation.Line as Line
import Annotation.Point as Point
import Annotation.Rectangle as Rectangle
import Data.Pointer as Pointer
import Json.Encode as Encode exposing (Value)



-- TYPES #############################################################


type Annotation
    = Point Point.Point
    | BBox Rectangle.Rectangle
    | Line Line.Line
    | UnfinishedOutline Line.Line
    | Outline Line.Line
    | UnfinishedPolygon Line.Line
    | Polygon Line.Line



-- UPDATE ############################################################


moveUpdate : Pointer.Msg -> Pointer.DragState -> Annotation -> Annotation
moveUpdate pointerMsg dragState annotation =
    case ( annotation, pointerMsg, dragState ) of
        ( Point point, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            Point (Point.fromCoordinates coordinates)

        ( BBox rect, Pointer.MoveAt coordinates, Pointer.DraggingFrom corner ) ->
            BBox (createBBox coordinates corner)

        ( Line line, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            Line (prependPointToLine coordinates line)

        ( UnfinishedOutline line, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            UnfinishedOutline (prependPointToLine coordinates line)

        ( UnfinishedPolygon line, Pointer.DownAt coordinates, Pointer.NoDrag ) ->
            UnfinishedPolygon (prependPointToLine coordinates line)

        ( UnfinishedPolygon (_ :: line), Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            UnfinishedPolygon (prependPointToLine coordinates line)

        _ ->
            Debug.todo "update annotation"


createBBox : ( Float, Float ) -> ( Float, Float ) -> Rectangle.Rectangle
createBBox corner1 corner2 =
    Rectangle.fromPointsPair ( Point.fromCoordinates corner1, Point.fromCoordinates corner2 )


prependPointToLine : ( Float, Float ) -> Line.Line -> Line.Line
prependPointToLine coordinates =
    Line.prependPoint (Point.fromCoordinates coordinates)



-- ENCODE ############################################################


encode : Annotation -> Value
encode annotation =
    case annotation of
        Point point ->
            Encode.object
                [ ( "type", Encode.string "point" )
                , ( "data", Point.encode point )
                ]

        BBox rectangle ->
            Encode.object
                [ ( "type", Encode.string "bbox" )
                , ( "data", Rectangle.encode rectangle )
                ]

        Line line ->
            Encode.object
                [ ( "type", Encode.string "line" )
                , ( "data", Line.encode line )
                ]

        UnfinishedOutline line ->
            Encode.object
                [ ( "type", Encode.string "unfinished-outline" )
                , ( "data", Line.encode line )
                ]

        Outline line ->
            Encode.object
                [ ( "type", Encode.string "outline" )
                , ( "data", Line.encode line )
                ]

        UnfinishedPolygon line ->
            Encode.object
                [ ( "type", Encode.string "unfinished-polygon" )
                , ( "data", Line.encode line )
                ]

        Polygon line ->
            Encode.object
                [ ( "type", Encode.string "polygon" )
                , ( "data", Line.encode line )
                ]
