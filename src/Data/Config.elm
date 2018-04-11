module Data.Config exposing (..)

import Data.Annotation as Annotation
import Data.Tool as Tool exposing (Tool)
import Json.Decode as Decode exposing (Decoder)
import Packages.StaticTreeMap as StaticTreeMap exposing (StaticTreeMap)
import Packages.Zipper as Zipper exposing (Zipper)
import Tree exposing (Tree)


-- TYPES #############################################################


type alias Config =
    { classes : List ClassConfig
    , annotations : List AnnotationConfig
    }


type ClassConfig
    = Label String
    | Category String (List ClassConfig)


type AnnotationConfig
    = Type Annotation.Type
    | TypeWithVariations Annotation.Type (List String)



-- FUNCTIONS #########################################################


empty : Config
empty =
    Config [] []


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
    , annotations =
        [ Type Annotation.Point
        , Type Annotation.BBox
        , TypeWithVariations Annotation.Stroke [ "fg", "bg" ]
        , Type Annotation.Outline
        , Type Annotation.Polygon
        ]
    }


classesFrom : List ClassConfig -> StaticTreeMap String
classesFrom classes =
    let
        convertNode : ClassConfig -> ( String, List ClassConfig )
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


toolsFrom : List AnnotationConfig -> Zipper Tool
toolsFrom configs =
    let
        moveTool : Tool
        moveTool =
            { id = 0
            , type_ = Tool.Move
            , variant = 0
            }

        zipperWithOnlyMove : Zipper Tool
        zipperWithOnlyMove =
            Zipper.init [] moveTool []

        addTools : AnnotationConfig -> Zipper Tool -> Zipper Tool
        addTools annotationConfig zipper =
            case annotationConfig of
                Type type_ ->
                    addVariations 0 type_ [] zipper

                TypeWithVariations type_ [] ->
                    addVariations 0 type_ [] zipper

                TypeWithVariations type_ (_ :: vs) ->
                    addVariations 1 type_ vs zipper

        addVariations : Int -> Annotation.Type -> List a -> Zipper Tool -> Zipper Tool
        addVariations variantId type_ otherVariations zipper =
            let
                tool =
                    { id = 1 + .id (Zipper.getC zipper)
                    , type_ = toolTypefromAnnotationType type_
                    , variant = variantId
                    }

                newZipper =
                    zipper
                        |> Zipper.insertR tool
                        |> Zipper.goR
            in
            case otherVariations of
                [] ->
                    newZipper

                v :: vs ->
                    addVariations (variantId + 1) type_ vs newZipper
    in
    List.foldl addTools zipperWithOnlyMove configs
        |> Zipper.goStart


toolTypefromAnnotationType : Annotation.Type -> Tool.Type
toolTypefromAnnotationType annotationType =
    case annotationType of
        Annotation.Point ->
            Tool.Point

        Annotation.BBox ->
            Tool.BBox

        Annotation.Stroke ->
            Tool.Stroke

        Annotation.Outline ->
            Tool.Outline

        Annotation.Polygon ->
            Tool.Polygon



-- Decoders


decoder : Decoder Config
decoder =
    Decode.map2 Config
        (Decode.field "classes" <| Decode.list classDecoder)
        (Decode.field "annotations" <| Decode.list annotationDecoder)


classDecoder : Decoder ClassConfig
classDecoder =
    Decode.oneOf
        [ Decode.map Label Decode.string
        , Decode.map2 Category (Decode.field "category" Decode.string) <|
            Decode.field "classes" <|
                Decode.list (Decode.lazy (\_ -> classDecoder))
        ]


annotationDecoder : Decoder AnnotationConfig
annotationDecoder =
    Decode.oneOf
        [ Decode.map Type Annotation.typeDecoder
        , Decode.map2 TypeWithVariations
            (Decode.field "type" Annotation.typeDecoder)
            (Decode.field "variations" <| Decode.list Decode.string)
        ]
