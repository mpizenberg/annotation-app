-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.AnnotatedImage exposing
    ( AnnotatedImage
    , AnnotatedImageUpdate
    , Status(..)
    , encode
    , fromRaw
    , hasAnnotations
    , reset
    )

import Data.Annotation as Annotation exposing (Annotation)
import Data.Image as Image exposing (Image)
import Data.Pointer as Pointer
import Data.RawImage as RawImage exposing (RawImage)
import Data.Tool as Tool exposing (Tool)
import Json.Encode as Encode exposing (Value)
import Packages.Zipper as Zipper exposing (Zipper)



-- TYPES #############################################################


type alias AnnotatedImage =
    { name : String
    , status : Status
    }


type Status
    = Loading
    | Loaded Image
    | LoadedWithAnnotations Image (Zipper { id : Int, annotation : Annotation ClassId })
    | LoadingError String


type alias ClassId =
    Int



-- FUNCTIONS #########################################################


reset : AnnotatedImage -> AnnotatedImage
reset annotatedImage =
    case annotatedImage.status of
        LoadedWithAnnotations img _ ->
            { annotatedImage | status = Loaded img }

        _ ->
            annotatedImage


hasAnnotations : AnnotatedImage -> Bool
hasAnnotations annotatedImage =
    case annotatedImage.status of
        LoadedWithAnnotations _ _ ->
            True

        _ ->
            False


focusUpdate : (Annotation ClassId -> Annotation ClassId) -> AnnotatedImage -> AnnotatedImage
focusUpdate f annotatedImage =
    case annotatedImage.status of
        LoadedWithAnnotations img zipper ->
            let
                newStatus =
                    LoadedWithAnnotations img (Zipper.updateC (updateAnnotation f) zipper)
            in
            { annotatedImage | status = newStatus }

        _ ->
            annotatedImage


updateAnnotation : (Annotation a -> Annotation a) -> { record | annotation : Annotation a } -> { record | annotation : Annotation a }
updateAnnotation f record =
    { record | annotation = f record.annotation }



-- Pointer stuff


addAnnotationsIndicator type_ ( list, dragState, hasChanged ) =
    Debug.todo "to remove"


type alias AnnotatedImageUpdate =
    { newAnnotatedImage : AnnotatedImage
    , newDragState : Pointer.DragState
    , hasAnnotations : Bool
    , hasChanged : Bool
    }



-- Conversion from raw image


fromRaw : Zipper Tool -> RawImage -> AnnotatedImage
fromRaw tools { id, name, status } =
    let
        annotatedStatus =
            case status of
                RawImage.Loading ->
                    Loading

                RawImage.LoadingError error ->
                    LoadingError error

                RawImage.Loaded image ->
                    Loaded image
    in
    { name = name, status = annotatedStatus }



-- Encoders


encode : AnnotatedImage -> Value
encode annotatedImage =
    Encode.object
        [ ( "image", Encode.string annotatedImage.name )
        , ( "annotations", encodeStatus annotatedImage.status )
        ]


encodeStatus : Status -> Value
encodeStatus status =
    case status of
        LoadedWithAnnotations image zipper ->
            Zipper.getAll zipper
                |> List.map .annotation
                |> Encode.list (Annotation.encode Encode.int)

        _ ->
            Encode.null
