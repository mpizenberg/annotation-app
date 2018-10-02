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


type Annotation a
    = Point Point.Point a
    | BBox Rectangle.Rectangle a
    | Line Line.Line a
    | UnfinishedOutline Line.Line a
    | Outline Line.Line a
    | UnfinishedPolygon Line.Line a
    | Polygon Line.Line a



-- UPDATE ############################################################


moveUpdate : Pointer.Msg -> Pointer.DragState -> Annotation a -> Annotation a
moveUpdate pointerMsg dragState annotation =
    case ( annotation, pointerMsg, dragState ) of
        ( Point point a, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            Point (Point.fromCoordinates coordinates) a

        ( BBox rect a, Pointer.MoveAt coordinates, Pointer.DraggingFrom corner ) ->
            BBox (createBBox coordinates corner) a

        ( Line line a, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            Line (prependPointToLine coordinates line) a

        ( UnfinishedOutline line a, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            UnfinishedOutline (prependPointToLine coordinates line) a

        ( UnfinishedPolygon line a, Pointer.DownAt coordinates, Pointer.NoDrag ) ->
            UnfinishedPolygon (prependPointToLine coordinates line) a

        ( UnfinishedPolygon (_ :: line) a, Pointer.MoveAt coordinates, Pointer.DraggingFrom _ ) ->
            UnfinishedPolygon (prependPointToLine coordinates line) a

        _ ->
            Debug.todo "update annotation"


createBBox : ( Float, Float ) -> ( Float, Float ) -> Rectangle.Rectangle
createBBox corner1 corner2 =
    Rectangle.fromPointsPair ( Point.fromCoordinates corner1, Point.fromCoordinates corner2 )


prependPointToLine : ( Float, Float ) -> Line.Line -> Line.Line
prependPointToLine coordinates =
    Line.prependPoint (Point.fromCoordinates coordinates)



-- ENCODE ############################################################


encode : (a -> Value) -> Annotation a -> Value
encode encodeClass annotation =
    case annotation of
        Point point a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "point" )
                , ( "data", Point.encode point )
                ]

        BBox rectangle a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "bbox" )
                , ( "data", Rectangle.encode rectangle )
                ]

        Line line a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "line" )
                , ( "data", Line.encode line )
                ]

        UnfinishedOutline line a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "unfinished-outline" )
                , ( "data", Line.encode line )
                ]

        Outline line a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "outline" )
                , ( "data", Line.encode line )
                ]

        UnfinishedPolygon line a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "unfinished-polygon" )
                , ( "data", Line.encode line )
                ]

        Polygon line a ->
            Encode.object
                [ ( "class", encodeClass a )
                , ( "type", Encode.string "polygon" )
                , ( "data", Line.encode line )
                ]
