-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Tool exposing
    ( Tool(..)
    , toString, fromString
    , toId, fromId
    , encode, decoder
    )

{-| Tools for the annotation application.

@docs Tool

@docs toString, fromString

@docs toId, fromId

@docs encode, decoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


{-| -}
type Tool
    = Move
    | Point
    | BBox
    | Line
    | Outline
    | Polygon


everyTool : List Tool
everyTool =
    [ Move, Point, BBox, Line, Outline, Polygon ]



-- From and to String description


{-| -}
toString : Tool -> String
toString tool =
    case tool of
        Move ->
            "move"

        Point ->
            "point"

        BBox ->
            "bbox"

        Line ->
            "line"

        Outline ->
            "outline"

        Polygon ->
            "polygon"


{-| -}
fromString : String -> Maybe Tool
fromString string =
    case string of
        "move" ->
            Just Move

        "point" ->
            Just Point

        "bbox" ->
            Just BBox

        "line" ->
            Just Line

        "outline" ->
            Just Outline

        "polygon" ->
            Just Polygon

        _ ->
            Nothing



-- From and to Int id


{-| -}
toId : Tool -> Int
toId tool =
    case tool of
        Move ->
            0

        Point ->
            1

        BBox ->
            2

        Line ->
            3

        Outline ->
            4

        Polygon ->
            5


{-| -}
fromId : Int -> Maybe Tool
fromId id =
    case id of
        0 ->
            Just Move

        1 ->
            Just Point

        2 ->
            Just BBox

        3 ->
            Just Line

        4 ->
            Just Outline

        5 ->
            Just Polygon

        _ ->
            Nothing



-- Encode / Decode


{-| -}
encode : Tool -> Value
encode =
    toString >> Encode.string


{-| -}
decoder : Decoder Tool
decoder =
    Decode.string
        |> Decode.map fromString
        |> Decode.andThen maybeDecoder


maybeDecoder : Maybe Tool -> Decoder Tool
maybeDecoder maybeTool =
    case maybeTool of
        Nothing ->
            List.map toString everyTool
                |> String.join " | "
                |> (++) "Error while decoding tool. A tool should be one of these: "
                |> Decode.fail

        Just tool ->
            Decode.succeed tool
