-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Annotation exposing
    ( Annotation(..)
    , encode
    , end
    , init
    , moveUpdate
    , prependPointToLine
    )

import Annotation.Line as Line
import Annotation.Point as Point
import Annotation.Rectangle as Rectangle
import Data.Pointer as Pointer
import Data.Tool as Tool exposing (Tool)
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


init : Tool -> ( Float, Float ) -> Maybe Annotation
init tool coordinates =
    case tool of
        Tool.Point ->
            Just <| Point (Point.fromCoordinates coordinates)

        Tool.BBox ->
            Just <| BBox (createBBox coordinates coordinates)

        Tool.Line ->
            Just <| Line [ Point.fromCoordinates coordinates ]

        Tool.Outline ->
            Just <| UnfinishedOutline [ Point.fromCoordinates coordinates ]

        Tool.Polygon ->
            Just <| UnfinishedPolygon [ Point.fromCoordinates coordinates ]

        _ ->
            Nothing



-- UPDATE ############################################################


moveUpdate : ( Float, Float ) -> Pointer.DragState -> Annotation -> Annotation
moveUpdate coordinates dragState annotation =
    case ( annotation, dragState ) of
        ( Point point, Pointer.DraggingFrom _ ) ->
            Point (Point.fromCoordinates coordinates)

        ( BBox rect, Pointer.DraggingFrom corner ) ->
            BBox (createBBox coordinates corner)

        ( Line line, Pointer.DraggingFrom _ ) ->
            Line (prependPointToLine coordinates line)

        ( UnfinishedOutline line, Pointer.DraggingFrom _ ) ->
            UnfinishedOutline (prependPointToLine coordinates line)

        ( UnfinishedPolygon (_ :: line), Pointer.DraggingFrom _ ) ->
            UnfinishedPolygon (prependPointToLine coordinates line)

        _ ->
            Debug.todo "update annotation"


createBBox : ( Float, Float ) -> ( Float, Float ) -> Rectangle.Rectangle
createBBox corner1 corner2 =
    Rectangle.fromPointsPair ( Point.fromCoordinates corner1, Point.fromCoordinates corner2 )


prependPointToLine : ( Float, Float ) -> Line.Line -> Line.Line
prependPointToLine coordinates =
    Line.prependPoint (Point.fromCoordinates coordinates)


end : Annotation -> Maybe Annotation
end annotation =
    Debug.todo "check after pointer up (remove, close, etc.)"



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
