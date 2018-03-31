module Annotation exposing (..)

import Annotation.Geometry.Types exposing (..)
import Json.Decode as Decode exposing (Decoder)


emptyConfig : Config
emptyConfig =
    { classes = []
    , kinds =
        [ Kind PointType [ "", "" ]
        , Kind BBoxType []
        ]
    }


type alias Config =
    { classes : List String
    , kinds : List Kind
    }


type alias Kind =
    { annotationType : Type
    , variants : List String
    }


type Type
    = PointType
    | BBoxType


type Annotations
    = Point PointDrawings
    | BBox BBoxDrawings


type alias PointDrawings =
    List Point


type alias StrokeDrawings =
    List Stroke


type alias BBoxDrawings =
    List BoundingBox


type OutlineDrawing
    = NoOutline
    | DrawingOutline Stroke
    | EndedOutline Outline


type ContourDrawing
    = NoContour
    | DrawingStartedAt ( Float, Float ) Stroke
    | Ended Contour



-- Decoders


configDecoder : Decoder Config
configDecoder =
    Decode.map2 Config
        (Decode.field "classes" <| Decode.list Decode.string)
        (Decode.field "kinds" <| Decode.list kindDecoder)


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

        _ ->
            PointType
