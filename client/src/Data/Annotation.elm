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
import Packages.ListExtra as ListExtra



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
            annotation


createBBox : ( Float, Float ) -> ( Float, Float ) -> Rectangle.Rectangle
createBBox corner1 corner2 =
    Rectangle.fromPointsPair ( Point.fromCoordinates corner1, Point.fromCoordinates corner2 )


prependPointToLine : ( Float, Float ) -> Line.Line -> Line.Line
prependPointToLine coordinates =
    Line.prependPoint (Point.fromCoordinates coordinates)


end : Float -> Annotation -> Maybe Annotation
end scale annotation =
    case annotation of
        -- Remove bounding box if all corners are the same point
        BBox rectangle ->
            if Rectangle.area rectangle == 0 then
                Nothing

            else
                Just annotation

        -- Remove line if it has only one point
        Line (_ :: []) ->
            Nothing

        -- Remove outline if it contains less than 3 points
        UnfinishedOutline (_ :: []) ->
            Nothing

        UnfinishedOutline (_ :: _ :: []) ->
            Nothing

        -- Close outline otherwise
        UnfinishedOutline ((_ :: _ :: _ :: _) as line) ->
            Just (Outline line)

        -- Only accept points that are different from previous for the first 3 points
        UnfinishedPolygon (p1 :: p2 :: []) ->
            if Point.distance p1 p2 / scale > samePointDistance then
                Just annotation

            else
                Just (UnfinishedPolygon [ p2 ])

        UnfinishedPolygon (p1 :: p2 :: p3 :: []) ->
            if Point.distance p1 p2 / scale > samePointDistance && Point.distance p1 p3 / scale > samePointDistance then
                Just annotation

            else
                Just (UnfinishedPolygon [ p2, p3 ])

        -- Close polygon if beginning and end match
        UnfinishedPolygon (p1 :: p2 :: p3 :: otherPoints) ->
            let
                pEnd =
                    ListExtra.last otherPoints
                        |> Maybe.withDefault p1
            in
            if Point.distance p1 pEnd / scale > samePointDistance then
                Just annotation

            else
                Just (Polygon (p2 :: p3 :: otherPoints))

        -- In any other case leave annotation untouched
        _ ->
            Just annotation


samePointDistance : Float
samePointDistance =
    10.0



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
