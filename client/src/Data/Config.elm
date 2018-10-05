-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Config exposing
    ( Config, Class, Error(..), empty, pascal
    , classesFrom
    , encode, decoder
    )

{-| Manage the app configuration

@docs Config, Class, Error, empty, pascal

@docs classesFrom

@docs encode, decoder

-}

import Data.Tool as Tool exposing (Tool)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Packages.FileSystem as FileSystem exposing (FileSystem)
import Packages.Zipper as Zipper exposing (Zipper)
import Tree.Zipper



-- TYPES #############################################################


{-| -}
type alias Config =
    { classes : List Class
    , tools : List Tool
    }


{-| -}
type Class
    = Label String
    | Category String (List Class)


{-| -}
type Error
    = IncorrectClasses
    | IncorrectTools
    | Incorrect Decode.Error



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
classesFrom : List Class -> FileSystem
classesFrom classes =
    let
        root =
            FileSystem.initWithFolder
                { id = 0
                , name = "root-category"
                , open = True
                , files = []
                }
    in
    classesFromAcc classes ( 1, root )
        |> Tuple.second


classesFromAcc : List Class -> ( Int, FileSystem ) -> ( Int, FileSystem )
classesFromAcc classes ( count, fileSystem ) =
    List.foldr accumClass ( count, fileSystem ) classes


accumClass : Class -> ( Int, FileSystem ) -> ( Int, FileSystem )
accumClass class ( count, parentFileSystem ) =
    case class of
        Label name ->
            ( count + 1, FileSystem.insertFile { id = count, name = name } parentFileSystem )

        Category name classes ->
            let
                systemWithCategory =
                    FileSystem.insertFolder
                        { id = count
                        , name = name
                        , open = True
                        , files = []
                        }
                        parentFileSystem

                subsystem =
                    Tree.Zipper.firstChild systemWithCategory
                        |> Maybe.withDefault systemWithCategory

                ( newCount, newSubsystem ) =
                    classesFromAcc classes ( count + 1, subsystem )
            in
            ( newCount, newSubsystem |> Tree.Zipper.parent |> Maybe.withDefault parentFileSystem )



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
        [ ( "classes", Encode.list encodeClass config.classes )
        , ( "tools", Encode.list Tool.encode config.tools )
        ]


encodeClass : Class -> Value
encodeClass class =
    case class of
        Label name ->
            Encode.string name

        Category name classes ->
            Encode.object
                [ ( "category", Encode.string name )
                , ( "classes", Encode.list encodeClass classes )
                ]
