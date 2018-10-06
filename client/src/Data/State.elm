-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.State exposing
    ( State(..), Classes, Error(..)
    , importFlagsImages
    , updateWithPointer
    , toggleCategory, selectClass, selectImage, selectTool
    , loadImages, imageLoaded
    , changeConfig
    , export
    , removeAnnotation
    )

{-| Module doc

@docs State, Classes, Error

@docs importFlagsImages

@docs updateWithPointer

@docs toggleCategory, selectClass, selectImage, selectTool

@docs loadImages, imageLoaded

@docs changeConfig

@docs export

@docs removeAnnotation

-}

import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.Config as Config exposing (Config)
import Data.Image exposing (Image)
import Data.Pointer as Pointer
import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Data.Tool as Tool exposing (Tool)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Packages.FileSystem as FileSystem exposing (FileSystem)
import Packages.Zipper as Zipper exposing (Zipper)
import Ports
import Viewer exposing (Viewer)


type State
    = NothingProvided Error
    | ConfigProvided Error Config Classes (Zipper Tool)
    | ImagesProvided Error Pointer.DragState RemoteZipper
    | AllProvided Error Pointer.DragState Config Classes (Zipper Tool) AnnotatedZipper


type alias Classes =
    { selected : Maybe ( Int, Int )
    , all : FileSystem
    }


type alias RemoteZipper =
    Zipper { id : Int, remoteImage : RemoteImage }


type alias AnnotatedZipper =
    Zipper { id : Int, annotatedImage : AnnotatedImage }


type Error
    = NoError
    | ConfigError Config.Error


importFlagsImages : List Image -> State
importFlagsImages images =
    case images of
        [] ->
            NothingProvided NoError

        firstImage :: otherImages ->
            let
                otherRemoteImagesWithId =
                    List.indexedMap (\id img -> remoteImageWithId (id + 1) img) otherImages
            in
            Zipper.init [] (remoteImageWithId 0 firstImage) otherRemoteImagesWithId
                |> ImagesProvided NoError Pointer.NoDrag


remoteImageWithId : Int -> Image -> { id : Int, remoteImage : RemoteImage }
remoteImageWithId id image =
    { id = id
    , remoteImage = RemoteImage image.url (RemoteImage.Loaded image)
    }



-- UPDATE ############################################################


selectImage : Int -> State -> State
selectImage id state =
    case state of
        ImagesProvided _ drag remoteImages ->
            ImagesProvided NoError drag (Zipper.goTo .id id remoteImages)

        AllProvided _ drag config classes tools images ->
            AllProvided NoError drag config classes tools (Zipper.goTo .id id images)

        _ ->
            state


toggleCategory : Int -> State -> State
toggleCategory id state =
    case state of
        AllProvided error drag config classes tools imgs ->
            FileSystem.findFolderWithId id classes.all
                |> Maybe.map toggleFocused
                |> Maybe.map (\all -> { classes | all = all })
                |> Maybe.map (\newClasses -> AllProvided error drag config newClasses tools imgs)
                |> Maybe.withDefault state

        _ ->
            state


toggleFocused : FileSystem -> FileSystem
toggleFocused =
    FileSystem.updateCurrentFolder (\f -> { f | open = not f.open })


selectClass : ( Int, Int ) -> State -> State
selectClass selected state =
    case state of
        AllProvided error drag config classes tools images ->
            AllProvided error drag config { classes | selected = Just selected } tools images

        _ ->
            state


selectTool : Int -> State -> State
selectTool id state =
    case state of
        ConfigProvided error config classes tools ->
            ConfigProvided error config classes (Zipper.goTo Tool.toId id tools)

        AllProvided error drag config classes tools imgs ->
            AllProvided error drag config classes (Zipper.goTo Tool.toId id tools) imgs

        _ ->
            state



-- Pointer movement


updateMove : Pointer.Msg -> Pointer.DragState -> Viewer -> ( Viewer, Pointer.DragState, Bool )
updateMove pointerMsg dragState viewer =
    case ( pointerMsg, dragState ) of
        ( Pointer.DownAt pos, _ ) ->
            ( viewer, Pointer.DraggingFrom pos, True )

        ( Pointer.MoveAt ( x, y ), Pointer.DraggingFrom ( ox, oy ) ) ->
            ( Viewer.pan ( x - ox, y - oy ) viewer, Pointer.DraggingFrom ( x, y ), True )

        ( Pointer.UpAt _, _ ) ->
            ( viewer, Pointer.NoDrag, True )

        _ ->
            ( viewer, dragState, False )


updateWithPointer : Pointer.Msg -> State -> State
updateWithPointer pointerMsg state =
    -- case state of
    --     AllProvided config classes tools imgs ->
    --         case Zipper.getC tools of
    --         Tool.Move ->
    --             let
    --                 ( newViewer, newDragState, hasChanged ) =
    --                     updateMove pointerMsg model.dragState model.viewer
    --             in
    --             if hasChanged then
    --                 ( { model | viewer = newViewer, dragState = newDragState }, Cmd.none )
    --
    --             else
    --                 ( model, Cmd.none )
    --
    --         _ ->
    --             let
    --                 scaledPointerMsg =
    --                     case pointerMsg of
    --                         Pointer.DownAt pos ->
    --                             Pointer.DownAt (Viewer.coordinatesAt pos model.viewer)
    --
    --                         Pointer.MoveAt pos ->
    --                             Pointer.MoveAt (Viewer.coordinatesAt pos model.viewer)
    --
    --                         Pointer.UpAt pos ->
    --                             Pointer.UpAt (Viewer.coordinatesAt pos model.viewer)
    --             in
    --             ( Debug.todo "update pointer", Cmd.none )
    Debug.todo "updateWithPointer"


removeAnnotation : State -> State
removeAnnotation state =
    case state of
        AllProvided NoError drag config classes tools imgs ->
            AllProvided NoError drag config classes tools (mapCurrentAnnotated AnnotatedImage.removeAnnotation imgs)

        _ ->
            state


mapCurrentAnnotated : (AnnotatedImage -> AnnotatedImage) -> AnnotatedZipper -> AnnotatedZipper
mapCurrentAnnotated f =
    Zipper.updateC (\c -> { c | annotatedImage = f c.annotatedImage })


loadImages : List { name : String, file : Value } -> State -> ( State, Cmd msg )
loadImages images state =
    case ( images, state ) of
        ( first :: files, NothingProvided _ ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneLoading setupRemoteLoading 0 first

                ( otherImages, otherCmds ) =
                    prepareListLoading setupRemoteLoading 1 files
            in
            ( ImagesProvided NoError Pointer.NoDrag (Zipper.init [] firstImage otherImages)
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( first :: files, ConfigProvided _ config classes tools ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneLoading setupAnnotatedLoading 0 first

                ( otherImages, otherCmds ) =
                    prepareListLoading setupAnnotatedLoading 1 files

                annotatedImages =
                    Zipper.init [] firstImage otherImages
            in
            ( AllProvided NoError Pointer.NoDrag config classes tools annotatedImages
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( files, ImagesProvided _ drag previousImages ) ->
            let
                startingId =
                    1 + .id (Zipper.getLast previousImages)

                ( newImages, cmds ) =
                    prepareListLoading setupRemoteLoading startingId files
            in
            ( ImagesProvided NoError drag (Zipper.append newImages previousImages)
            , Cmd.batch cmds
            )

        ( files, AllProvided _ drag config classes tools previousImages ) ->
            let
                startingId =
                    1 + .id (Zipper.getLast previousImages)

                ( newImages, cmds ) =
                    prepareListLoading setupAnnotatedLoading startingId files

                annotatedImages =
                    Zipper.append newImages previousImages
            in
            ( AllProvided NoError drag config classes tools annotatedImages
            , Cmd.batch cmds
            )

        _ ->
            ( state, Cmd.none )


mapCurrentRemote : (RemoteImage -> RemoteImage) -> RemoteZipper -> RemoteZipper
mapCurrentRemote f =
    Zipper.updateC (\c -> { c | remoteImage = f c.remoteImage })


imageLoaded : { id : Int, url : String, width : Int, height : Int } -> State -> State
imageLoaded { id, url, width, height } state =
    case state of
        ImagesProvided error drag images ->
            Zipper.goTo .id id images
                |> mapCurrentRemote (\r -> { r | status = RemoteImage.Loaded (Image url width height) })
                |> Zipper.goTo .id (.id (Zipper.getC images))
                |> ImagesProvided error drag

        AllProvided error drag config classes tools images ->
            Zipper.goTo .id id images
                |> mapCurrentAnnotated (\r -> { r | status = AnnotatedImage.Loaded (Image url width height) })
                |> Zipper.goTo .id (.id (Zipper.getC images))
                |> AllProvided error drag config classes tools

        _ ->
            state



-- Config loading


changeConfig : String -> State -> State
changeConfig configString state =
    case ( state, decodeConfig configString ) of
        -- No error
        ( ImagesProvided _ drag images, Ok ( config, classes, tools ) ) ->
            AllProvided NoError drag config classes tools (Zipper.mapAll upgradeImage images)

        ( AllProvided _ drag _ _ _ images, Ok ( config, classes, tools ) ) ->
            AllProvided NoError drag config classes tools (Zipper.mapAll resetImage images)

        ( _, Ok ( config, classes, tools ) ) ->
            ConfigProvided NoError config classes tools

        -- Error
        ( NothingProvided _, Err configError ) ->
            NothingProvided (ConfigError configError)

        ( ConfigProvided _ config classes tools, Err configError ) ->
            ConfigProvided (ConfigError configError) config classes tools

        ( ImagesProvided _ drag images, Err configError ) ->
            ImagesProvided (ConfigError configError) drag images

        ( AllProvided _ drag config classes tools images, Err configError ) ->
            AllProvided (ConfigError configError) drag config classes tools images


decodeConfig : String -> Result Config.Error ( Config, Classes, Zipper Tool )
decodeConfig configString =
    case Decode.decodeString Config.decoder configString of
        Ok config ->
            case config.tools of
                [] ->
                    Err Config.IncorrectTools

                firstTool :: otherTools ->
                    Ok
                        ( config
                        , { selected = Nothing, all = Config.classesFrom config.classes }
                        , Zipper.init [] firstTool otherTools
                        )

        Err decodeError ->
            Err (Config.Incorrect decodeError)


upgradeImage : { id : Int, remoteImage : RemoteImage } -> { id : Int, annotatedImage : AnnotatedImage }
upgradeImage { id, remoteImage } =
    { id = id, annotatedImage = AnnotatedImage.fromRemote remoteImage }


resetImage : { a | annotatedImage : AnnotatedImage } -> { a | annotatedImage : AnnotatedImage }
resetImage a =
    { a | annotatedImage = AnnotatedImage.reset a.annotatedImage }



-- Images loading


setupRemoteLoading : Int -> String -> { id : Int, remoteImage : RemoteImage }
setupRemoteLoading id name =
    { id = id, remoteImage = { name = name, status = RemoteImage.Loading } }


setupAnnotatedLoading : Int -> String -> { id : Int, annotatedImage : AnnotatedImage }
setupAnnotatedLoading id name =
    { id = id, annotatedImage = { name = name, status = AnnotatedImage.Loading } }


prepareListLoading :
    (Int -> String -> loading)
    -> Int
    -> List { name : String, file : Value }
    -> ( List loading, List (Cmd msg) )
prepareListLoading setup startId images =
    List.indexedMap (\id img -> prepareOneLoading setup (startId + id) img) images
        |> List.unzip


prepareOneLoading : (Int -> String -> loading) -> Int -> { name : String, file : Value } -> ( loading, Cmd msg )
prepareOneLoading setup id { name, file } =
    ( setup id name
    , Ports.loadImageFile { id = id, file = file }
    )



-- Export / save annotations


export : State -> Cmd msg
export state =
    case state of
        AllProvided _ _ config _ _ images ->
            Ports.export <| encode config <| List.map .annotatedImage <| Zipper.getAll images

        _ ->
            Cmd.none


encode : Config -> List AnnotatedImage -> Value
encode config images =
    Encode.object
        [ ( "config", Config.encode config )
        , ( "images", Encode.list AnnotatedImage.encode images )
        ]
