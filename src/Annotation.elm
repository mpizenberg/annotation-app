module Annotation exposing (..)

import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Types exposing (..)
import Json.Decode as Decode exposing (Decoder)


emptyConfig : Config
emptyConfig =
    { classes = []
    , kinds = []
    }


defaultConfig : Config
defaultConfig =
    { classes =
        [ ClassItem "mineral"
        , ClassCategory "living"
            [ ClassItem "plant"
            , ClassCategory "animal"
                [ ClassItem "cat"
                , ClassItem "dog"
                ]
            ]
        ]
    , kinds =
        [ Kind PointType [ "fg", "bg" ]
        , Kind BBoxType []
        ]
    }


type alias Config =
    { classes : List ClassesConfig
    , kinds : List Kind
    }


type ClassesConfig
    = ClassItem String
    | ClassCategory String (List ClassesConfig)


type alias Kind =
    { annotationType : Type
    , variants : List String
    }


type Type
    = PointType
    | BBoxType
    | StrokeType
    | OutlineType
    | PolygonType


type Annotations
    = Point PointDrawings
    | BBox BBoxDrawings
    | Stroke StrokeDrawings
    | Outline OutlineDrawings
    | Polygon PolygonDrawings


type alias PointDrawings =
    List Point


type alias StrokeDrawings =
    List Stroke


type alias BBoxDrawings =
    List ( Int, BoundingBox )


type alias OutlineDrawings =
    List OutlineDrawing


type OutlineDrawing
    = DrawingOutline Stroke
    | EndedOutline Outline


type alias PolygonDrawings =
    List PolygonDrawing


type PolygonDrawing
    = PolygonStartedAt ( Float, Float ) Stroke
    | EndedPolygon Contour



-- Decoders


configDecoder : Decoder Config
configDecoder =
    Decode.map2 Config
        (Decode.field "classes" <| Decode.list classesDecoder)
        (Decode.field "kinds" <| Decode.list kindDecoder)


classesDecoder : Decoder ClassesConfig
classesDecoder =
    Decode.oneOf
        [ Decode.map ClassItem Decode.string
        , Decode.map2 ClassCategory (Decode.field "category" Decode.string) <|
            Decode.field "items" <|
                Decode.list <|
                    Decode.lazy (\_ -> classesDecoder)
        ]


kindDecoder : Decoder Kind
kindDecoder =
    Decode.map2 Kind
        (Decode.field "type" typeDecoder)
        (Decode.field "variants" <| Decode.list Decode.string)


typeDecoder : Decoder Type
typeDecoder =
    Decode.map typeFromString Decode.string


typeFromString : String -> Type
typeFromString str =
    case str of
        "point" ->
            PointType

        "bbox" ->
            BBoxType

        "stroke" ->
            StrokeType

        "outline" ->
            OutlineType

        "polygon" ->
            PolygonType

        _ ->
            PointType



-- Utils


hasAnnotation : Annotations -> Bool
hasAnnotation annotations =
    case annotations of
        Point drawings ->
            not (List.isEmpty drawings)

        BBox drawings ->
            not (List.isEmpty drawings)

        Stroke drawings ->
            not (List.isEmpty drawings)

        Outline drawings ->
            not (List.isEmpty drawings)

        Polygon drawings ->
            not (List.isEmpty drawings)


removeLast : Annotations -> Annotations
removeLast annotations =
    case annotations of
        Point drawings ->
            Point <| List.drop 1 drawings

        BBox drawings ->
            BBox <| List.drop 1 drawings

        Stroke drawings ->
            Stroke <| List.drop 1 drawings

        Outline drawings ->
            Outline <| List.drop 1 drawings

        Polygon drawings ->
            Polygon <| List.drop 1 drawings



-- Updates


type DragState
    = NoDrag
    | DraggingFrom ( Float, Float )


type alias Position =
    ( Float, Float )


type PointerMsg
    = PointerDownAt ( Float, Float )
    | PointerMoveAt ( Float, Float )
    | PointerUpAt ( Float, Float )


updatePoints : (Position -> Position) -> PointerMsg -> DragState -> PointDrawings -> ( PointDrawings, DragState )
updatePoints scaling pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( PointerDownAt pos, NoDrag, _ ) ->
            let
                scaledPos =
                    scaling pos
            in
            ( Point.fromCoordinates scaledPos :: drawings, DraggingFrom scaledPos )

        ( PointerMoveAt pos, DraggingFrom _, point :: points ) ->
            ( Point.fromCoordinates (scaling pos) :: points, dragState )

        ( PointerUpAt _, _, _ ) ->
            ( drawings, NoDrag )

        _ ->
            ( drawings, dragState )


updateBBox : Int -> (Position -> Position) -> PointerMsg -> DragState -> BBoxDrawings -> ( BBoxDrawings, DragState )
updateBBox classId scaling pointerMsg dragState drawings =
    case ( pointerMsg, dragState, drawings ) of
        ( PointerDownAt pos, NoDrag, _ ) ->
            let
                scaledPos =
                    scaling pos

                point =
                    Point.fromCoordinates scaledPos
            in
            ( ( classId, BoundingBox.fromPair ( point, point ) ) :: drawings
            , DraggingFrom scaledPos
            )

        ( PointerMoveAt pos, DraggingFrom scaledFirstCorner, ( id, bbox ) :: bboxes ) ->
            ( ( id
              , BoundingBox.fromPair
                    ( Point.fromCoordinates scaledFirstCorner
                    , Point.fromCoordinates (scaling pos)
                    )
              )
                :: bboxes
            , dragState
            )

        ( PointerUpAt _, _, _ ) ->
            ( drawings, NoDrag )

        _ ->
            ( drawings, dragState )
