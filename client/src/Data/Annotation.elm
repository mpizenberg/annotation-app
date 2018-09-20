-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Annotation exposing
    ( Info
    , Type(..)
    , encodeBBox
    , encodeOutline
    , encodePoint
    , encodePolygon
    , encodeStroke
    , typeDecoder
    , typeFromString
    , typeToString
    )

import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Contour as Contour
import Annotation.Geometry.Outline as Outline
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Stroke as Stroke
import Annotation.Geometry.Types as Geometry
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES #############################################################


type Type
    = Point
    | BBox
    | Stroke
    | Outline
    | Polygon


type alias Info =
    { type_ : Type
    , variant : Maybe String
    }



-- FUNCTIONS #########################################################


typeFromString : String -> Type
typeFromString str =
    case str of
        "point" ->
            Point

        "bbox" ->
            BBox

        "stroke" ->
            Stroke

        "outline" ->
            Outline

        "polygon" ->
            Polygon

        _ ->
            Point


typeToString : Type -> String
typeToString type_ =
    case type_ of
        Point ->
            "point"

        BBox ->
            "bbox"

        Stroke ->
            "stroke"

        Outline ->
            "outline"

        Polygon ->
            "polygon"



-- Decoders


typeDecoder : Decoder Type
typeDecoder =
    Decode.map typeFromString Decode.string



-- Encoders


encodePoint : Geometry.Point -> Value
encodePoint point =
    Point.encode point


encodeBBox : Geometry.BoundingBox -> Value
encodeBBox bbox =
    BoundingBox.encode bbox


encodeStroke : Geometry.Stroke -> Value
encodeStroke stroke =
    Stroke.encode stroke


encodeOutline : Geometry.Outline -> Value
encodeOutline outline =
    Outline.encode outline


encodePolygon : Geometry.Contour -> Value
encodePolygon polygon =
    Contour.encode polygon
