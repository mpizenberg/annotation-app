-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.AnnotatedImage exposing
    ( AnnotatedImage
    , Status(..)
    , encode
    , fromRemote
    , hasAnnotations
    , removeAnnotation
    , reset
    , updateWithPointer
    )

import Data.Annotation as Annotation exposing (Annotation)
import Data.Image as Image exposing (Image)
import Data.Pointer as Pointer
import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Data.Tool as Tool exposing (Tool)
import Json.Encode as Encode exposing (Value)
import Packages.Zipper as Zipper exposing (Zipper(..))



-- TYPES #############################################################


type alias AnnotatedImage =
    { name : String
    , status : Status
    }


type Status
    = Loading
    | Loaded Image
    | LoadedWithAnnotations Image Int (Zipper AnnotationWithId)
    | LoadingError String


type alias AnnotationWithId =
    { id : Int
    , classId : Int
    , annotation : Annotation
    }



-- FUNCTIONS #########################################################


reset : AnnotatedImage -> AnnotatedImage
reset annotatedImage =
    case annotatedImage.status of
        LoadedWithAnnotations img _ _ ->
            { annotatedImage | status = Loaded img }

        _ ->
            annotatedImage


hasAnnotations : AnnotatedImage -> Bool
hasAnnotations annotatedImage =
    case annotatedImage.status of
        LoadedWithAnnotations _ _ _ ->
            True

        _ ->
            False


removeAnnotation : AnnotatedImage -> AnnotatedImage
removeAnnotation annotatedImage =
    case annotatedImage.status of
        LoadedWithAnnotations img count zipper ->
            case Zipper.removeGoRelseL zipper of
                Just newZipper ->
                    { annotatedImage | status = LoadedWithAnnotations img count newZipper }

                _ ->
                    { annotatedImage | status = Loaded img }

        _ ->
            annotatedImage


updateWithPointer : Pointer.Msg -> Pointer.DragState -> Tool -> Int -> AnnotatedImage -> AnnotatedImage
updateWithPointer pointerMsg dragState tool classId ({ name } as annotatedImage) =
    case ( pointerMsg, annotatedImage.status ) of
        ( Pointer.DownAt pos, _ ) ->
            updateWithPointerDownAt pos tool classId annotatedImage

        ( Pointer.MoveAt pos, LoadedWithAnnotations img count zipper ) ->
            updateCurrentWith (Annotation.moveUpdate pos dragState) name img count zipper

        ( Pointer.UpAt _, LoadedWithAnnotations img count zipper ) ->
            checkCurrent name img count zipper

        _ ->
            annotatedImage


updateWithPointerDownAt : ( Float, Float ) -> Tool -> Int -> AnnotatedImage -> AnnotatedImage
updateWithPointerDownAt coordinates tool classId annotatedImage =
    case ( tool, annotatedImage.status ) of
        ( Tool.Polygon, LoadedWithAnnotations img count zipper ) ->
            case .annotation (Zipper.getC zipper) of
                Annotation.UnfinishedPolygon line ->
                    Annotation.UnfinishedPolygon (Annotation.prependPointToLine coordinates line)
                        |> changeCurrentOf zipper
                        |> LoadedWithAnnotations img count
                        |> (\status -> { annotatedImage | status = status })

                _ ->
                    annotatedImage

        ( _, Loaded img ) ->
            case Annotation.init tool coordinates of
                Just annotation ->
                    { id = 0, classId = classId, annotation = annotation }
                        |> Zipper.singleton
                        |> LoadedWithAnnotations img 0
                        |> (\status -> { annotatedImage | status = status })

                Nothing ->
                    annotatedImage

        ( _, LoadedWithAnnotations img count zipper ) ->
            case Annotation.init tool coordinates of
                Just annotation ->
                    appendAnnotation (count + 1) classId annotation zipper
                        |> LoadedWithAnnotations img (count + 1)
                        |> (\status -> { annotatedImage | status = status })

                Nothing ->
                    annotatedImage

        _ ->
            annotatedImage


changeCurrentOf : Zipper AnnotationWithId -> Annotation -> Zipper AnnotationWithId
changeCurrentOf zipper annotation =
    Zipper.updateC (\current -> { current | annotation = annotation }) zipper


appendAnnotation : Int -> Int -> Annotation -> Zipper AnnotationWithId -> Zipper AnnotationWithId
appendAnnotation id classId annotation zipper =
    Zipper.goEnd zipper
        |> Zipper.insertGoR (AnnotationWithId id classId annotation)


checkCurrent : String -> Image -> Int -> Zipper AnnotationWithId -> AnnotatedImage
checkCurrent name img count zipper =
    case Annotation.end (.annotation (Zipper.getC zipper)) of
        Just updatedAnnotation ->
            changeCurrentOf zipper updatedAnnotation
                |> LoadedWithAnnotations img count
                |> (\status -> { name = name, status = status })

        Nothing ->
            case Zipper.removeGoRelseL zipper of
                Just newZipper ->
                    { name = name, status = LoadedWithAnnotations img count newZipper }

                _ ->
                    { name = name, status = Loaded img }


updateCurrentWith : (Annotation -> Annotation) -> String -> Image -> Int -> Zipper AnnotationWithId -> AnnotatedImage
updateCurrentWith f imageName img count zipper =
    { name = imageName
    , status = LoadedWithAnnotations img count (Zipper.updateC (updateAnnotationWith f) zipper)
    }


updateAnnotationWith : (Annotation -> Annotation) -> AnnotationWithId -> AnnotationWithId
updateAnnotationWith f withId =
    { withId | annotation = f withId.annotation }



-- Conversion from remote image


fromRemote : RemoteImage -> AnnotatedImage
fromRemote { name, status } =
    let
        annotatedStatus =
            case status of
                RemoteImage.Loading ->
                    Loading

                RemoteImage.LoadingError error ->
                    LoadingError error

                RemoteImage.Loaded image ->
                    Loaded image
    in
    { name = name, status = annotatedStatus }



-- Encoders


encode : AnnotatedImage -> Value
encode { name, status } =
    case status of
        LoadedWithAnnotations img _ zipper ->
            Encode.object
                [ ( "image", Encode.string name )
                , ( "size", Encode.list Encode.int [ img.width, img.height ] )
                , ( "annotations", Encode.list encodeAnnotationWithId (Zipper.getAll zipper) )
                ]

        _ ->
            Encode.object
                [ ( "image", Encode.string name )
                , ( "annotations", Encode.null )
                ]


encodeAnnotationWithId : AnnotationWithId -> Value
encodeAnnotationWithId { id, classId, annotation } =
    Encode.object
        [ ( "classId", Encode.int classId )
        , ( "annotation", Annotation.encode annotation )
        ]
