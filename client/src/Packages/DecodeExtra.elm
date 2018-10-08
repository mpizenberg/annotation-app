module Packages.DecodeExtra exposing (each, indexedList)

import Json.Decode as Decode exposing (Decoder)


indexedList : Decoder a -> Decoder (List a)
indexedList decoder =
    Decode.field "length" Decode.int
        |> Decode.andThen (\length -> fieldRange 0 (length - 1) decoder)


fieldRange : Int -> Int -> Decoder a -> Decoder (List a)
fieldRange start end decoder =
    List.range start end
        |> List.map (\n -> indexField n decoder)
        |> each


each : List (Decoder a) -> Decoder (List a)
each =
    List.foldr (Decode.map2 (::)) (Decode.succeed [])


indexField : Int -> Decoder a -> Decoder a
indexField n =
    Decode.field (String.fromInt n)
