-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Feature exposing
    ( Feature(..)
    , toString, fromString
    , encode, decoder
    )

{-| Custom features for the annotation application.

@docs Feature

@docs toString, fromString

@docs encode, decoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


{-| -}
type Feature
    = CanRemoveAnnotation
    | CanZoom


everyFeature : List Feature
everyFeature =
    [ CanRemoveAnnotation, CanZoom ]



-- From and to String description


{-| -}
toString : Feature -> String
toString feature =
    case feature of
        CanRemoveAnnotation ->
            "remove-annotation"

        CanZoom ->
            "zoom"


{-| -}
fromString : String -> Maybe Feature
fromString str =
    case str of
        "remove-annotation" ->
            Just CanRemoveAnnotation

        "zoom" ->
            Just CanZoom

        _ ->
            Nothing



-- Encode / Decode


{-| -}
encode : Feature -> Value
encode feature =
    Encode.string (toString feature)


{-| -}
decoder : Decoder Feature
decoder =
    Decode.string
        |> Decode.map fromString
        |> Decode.andThen maybeDecoder


maybeDecoder : Maybe Feature -> Decoder Feature
maybeDecoder maybeFeature =
    case maybeFeature of
        Nothing ->
            List.map toString everyFeature
                |> String.join " | "
                |> (++) "Error while decoding feature. A feature should be one of these: "
                |> Decode.fail

        Just feature ->
            Decode.succeed feature
