-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Config exposing
    ( Config, Class, empty, pascal
    , classesFrom, toolsZipperFromConfig
    , encode, decoder
    )

{-| Manage the app configuration

@docs Config, Class, empty, pascal

@docs classesFrom, toolsZipperFromConfig

@docs encode, decoder

-}

import Data.Tool as Tool exposing (Tool)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Packages.StaticTreeMap as StaticTreeMap exposing (StaticTreeMap)
import Packages.Zipper as Zipper exposing (Zipper)
import Tree exposing (Tree)



-- TYPES #############################################################


{-| -}
type alias Config =
    { classes : List Class
    , tools : List Tool
    }


type Class
    = Label String
    | Category String (List Class)



-- FUNCTIONS #########################################################


{-| -}
empty : Config
empty =
    Config [] []


{-| -}
pascal : Config
pascal =
    { classes =
        [ Label "person"
        , Category "Animal"
            [ Label "bird"
            , Label "cat"
            , Label "cow"
            , Label "dog"
            , Label "horse"
            , Label "sheep"
            ]
        , Category "Vehicle"
            [ Label "aeroplane"
            , Label "bicycle"
            , Label "boat"
            , Label "bus"
            , Label "car"
            , Label "motorbike"
            , Label "train"
            ]
        , Category "Indoor"
            [ Label "bottle"
            , Label "chair"
            , Label "dining table"
            , Label "potted plant"
            , Label "sofa"
            , Label "tv/monitor"
            ]
        ]
    , tools =
        [ Tool.Move
        , Tool.BBox
        , Tool.Line
        , Tool.Outline
        , Tool.Polygon
        ]
    }


{-| -}
classesFrom : List Class -> StaticTreeMap String
classesFrom classes =
    let
        convertNode : Class -> ( String, List Class )
        convertNode class =
            case class of
                Label name ->
                    ( name, [] )

                Category name subClasses ->
                    ( name, subClasses )
    in
    Category "classes-root" classes
        |> Tree.unfold convertNode
        |> StaticTreeMap.from


{-| -}
toolsZipperFromConfig : List Tool -> Maybe (Zipper Tool)
toolsZipperFromConfig tools =
    Nothing



-- Decoders


{-| -}
decoder : Decoder Config
decoder =
    Decode.map2 Config
        (Decode.field "classes" <| Decode.list classDecoder)
        (Decode.field "tools" <| Decode.list Tool.decoder)


classDecoder : Decoder Class
classDecoder =
    Decode.oneOf
        [ Decode.map Label Decode.string
        , Decode.map2 Category (Decode.field "category" Decode.string) <|
            Decode.field "classes" <|
                Decode.list (Decode.lazy (\_ -> classDecoder))
        ]



-- Encoders


{-| -}
encode : Config -> Value
encode config =
    Encode.object
        [ ( "classes", Encode.list <| List.map encodeClass config.classes )
        , ( "tools", Encode.list <| List.map Tool.encode config.tools )
        ]


encodeClass : Class -> Value
encodeClass class =
    case class of
        Label name ->
            Encode.string name

        Category name classes ->
            Encode.object
                [ ( "category", Encode.string name )
                , ( "classes", Encode.list <| List.map encodeClass classes )
                ]
