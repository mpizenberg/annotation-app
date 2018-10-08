-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Packages.FileInput exposing (Config, Quantity(..), invisible)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Packages.DecodeExtra as DecodeExtra


type alias Config msg =
    { id : String
    , quantity : Quantity msg
    , accept : String
    }


type Quantity msg
    = SingleWith (Value -> msg)
    | MultipleWith (List { name : String, file : Value } -> msg)


invisible : Config msg -> Html msg
invisible config =
    let
        ( multipleAttribute, onChangeAttribute ) =
            case config.quantity of
                SingleWith msgBuilder ->
                    ( Html.Attributes.multiple False
                    , loadSingleFileOnChange msgBuilder
                    )

                MultipleWith msgBuilder ->
                    ( Html.Attributes.multiple True
                    , loadMultipleFilesOnChange msgBuilder
                    )
    in
    Html.input
        [ Html.Attributes.id config.id
        , Html.Attributes.type_ "file"
        , Html.Attributes.style "display" "none"
        , Html.Attributes.accept config.accept
        , multipleAttribute
        , onChangeAttribute
        ]
        []


loadSingleFileOnChange : (Value -> msg) -> Html.Attribute msg
loadSingleFileOnChange msgBuilder =
    Decode.at [ "target", "files", "0" ] Decode.value
        |> Decode.map
            (\file ->
                { message = msgBuilder file
                , stopPropagation = False
                , preventDefault = True
                }
            )
        |> Html.Events.custom "change"


loadMultipleFilesOnChange : (List { name : String, file : Value } -> msg) -> Html.Attribute msg
loadMultipleFilesOnChange msgBuilder =
    Decode.at [ "target", "files" ] (DecodeExtra.indexedList namedFileDecoder)
        |> Decode.map
            (\files ->
                { message = msgBuilder files
                , stopPropagation = False
                , preventDefault = True
                }
            )
        |> Html.Events.custom "change"



-- Decoder


namedFileDecoder : Decoder { name : String, file : Value }
namedFileDecoder =
    Decode.map2 namedFile (Decode.field "name" Decode.string) Decode.value


namedFile : String -> Value -> { name : String, file : Value }
namedFile name value =
    { name = name, file = value }
