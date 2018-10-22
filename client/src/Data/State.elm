-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.State exposing
    ( State(..), Error(..), Classes, SidePanels, RemoteZipper, AnnotatedZipper
    , importFlagsImages
    , updateWithPointer
    , toggleCategory, selectClass, selectImage, selectTool, toggleClassesPanel, toggleImagesPanel
    , loadImages, imageLoaded
    , changeConfig
    , export
    , removeAnnotation
    )

{-| Module doc

@docs State, Error, Classes, SidePanels, RemoteZipper, AnnotatedZipper

@docs importFlagsImages

@docs updateWithPointer

@docs toggleCategory, selectClass, selectImage, selectTool, toggleClassesPanel, toggleImagesPanel

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
    | ConfigProvided Error Config Bool Classes (Zipper Tool)
    | ImagesProvided Error Pointer.DragState Bool RemoteZipper
    | AllProvided Error Pointer.DragState Config SidePanels Classes (Zipper Tool) AnnotatedZipper


type alias Classes =
    { selected : Maybe ( Int, Int )
    , all : FileSystem
    }


type alias SidePanels =
    { imagesVisible : Bool
    , classesVisible : Bool
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
                |> ImagesProvided NoError Pointer.NoDrag True


remoteImageWithId : Int -> Image -> { id : Int, remoteImage : RemoteImage }
remoteImageWithId id image =
    { id = id
    , remoteImage = RemoteImage image.url (RemoteImage.Loaded image)
    }



-- UPDATE ############################################################


selectImage : Int -> Viewer -> State -> ( State, Viewer )
selectImage id viewer state =
    case state of
        ImagesProvided _ drag visible remoteImages ->
            let
                movedZipper =
                    Zipper.goTo .id id remoteImages

                newViewer =
                    case .status (.remoteImage (Zipper.getC movedZipper)) of
                        RemoteImage.Loaded img ->
                            Viewer.fitImage 1.2 ( toFloat img.width, toFloat img.height ) viewer

                        _ ->
                            viewer
            in
            ( ImagesProvided NoError drag visible movedZipper, newViewer )

        AllProvided _ drag config sidePanels classes tools images ->
            let
                movedZipper =
                    Zipper.goTo .id id images

                newViewer =
                    case .status (.annotatedImage (Zipper.getC movedZipper)) of
                        AnnotatedImage.Loaded img ->
                            Viewer.fitImage 1.2 ( toFloat img.width, toFloat img.height ) viewer

                        AnnotatedImage.LoadedWithAnnotations img _ _ ->
                            Viewer.fitImage 1.2 ( toFloat img.width, toFloat img.height ) viewer

                        _ ->
                            viewer
            in
            ( AllProvided NoError drag config sidePanels classes tools movedZipper, newViewer )

        _ ->
            ( state, viewer )


toggleCategory : Int -> State -> State
toggleCategory id state =
    case state of
        ConfigProvided error config visible classes tools ->
            FileSystem.findFolderWithId id classes.all
                |> Maybe.map toggleFocused
                |> Maybe.map (\all -> { classes | all = all })
                |> Maybe.map (\newClasses -> ConfigProvided error config visible newClasses tools)
                |> Maybe.withDefault state

        AllProvided error drag config sidePanels classes tools imgs ->
            FileSystem.findFolderWithId id classes.all
                |> Maybe.map toggleFocused
                |> Maybe.map (\all -> { classes | all = all })
                |> Maybe.map (\newClasses -> AllProvided error drag config sidePanels newClasses tools imgs)
                |> Maybe.withDefault state

        _ ->
            state


toggleFocused : FileSystem -> FileSystem
toggleFocused =
    FileSystem.updateCurrentFolder (\f -> { f | open = not f.open })


toggleClassesPanel : State -> State
toggleClassesPanel state =
    case state of
        ConfigProvided error config visible classes tools ->
            ConfigProvided error config (not visible) classes tools

        AllProvided error drag config sidePanels classes tools imgs ->
            let
                toggledSidePanels =
                    { sidePanels | classesVisible = not sidePanels.classesVisible }
            in
            AllProvided error drag config toggledSidePanels classes tools imgs

        _ ->
            Debug.todo "toggleClassesPanel"


toggleImagesPanel : State -> State
toggleImagesPanel state =
    case state of
        ImagesProvided error drag visible remoteImages ->
            ImagesProvided error drag (not visible) remoteImages

        AllProvided error drag config sidePanels classes tools imgs ->
            let
                toggledSidePanels =
                    { sidePanels | imagesVisible = not sidePanels.imagesVisible }
            in
            AllProvided error drag config toggledSidePanels classes tools imgs

        _ ->
            Debug.todo "toggleImagesPanel"


selectClass : ( Int, Int ) -> State -> State
selectClass selected state =
    case state of
        ConfigProvided error config visible classes tools ->
            ConfigProvided error config visible { classes | selected = Just selected } tools

        AllProvided error drag config sidePanels classes tools images ->
            AllProvided error drag config sidePanels { classes | selected = Just selected } tools images

        _ ->
            state


selectTool : Int -> State -> State
selectTool id state =
    case state of
        ConfigProvided error config visible classes tools ->
            ConfigProvided error config visible classes (Zipper.goTo Tool.toId id tools)

        AllProvided error drag config sidePanels classes tools imgs ->
            AllProvided error drag config sidePanels classes (Zipper.goTo Tool.toId id tools) imgs

        _ ->
            state



-- Pointer movement


updateWithPointer : Pointer.Msg -> Viewer -> State -> Maybe ( State, Viewer )
updateWithPointer pointerMsg viewer state =
    case state of
        AllProvided error drag config sidePanels classes tools imgs ->
            case ( Zipper.getC tools, classes.selected ) of
                ( Tool.Move, _ ) ->
                    updateMove pointerMsg drag viewer
                        |> Maybe.map (Tuple.mapFirst (dragToState error config sidePanels classes tools imgs))

                ( tool, Just ( _, classId ) ) ->
                    let
                        scaledPointerMsg =
                            Pointer.mapMsg (\pos -> Viewer.coordinatesAt pos viewer) pointerMsg

                        newDrag =
                            Pointer.update pointerMsg drag

                        updateAnnotated =
                            AnnotatedImage.updateWithPointer scaledPointerMsg newDrag tool classId viewer.scale
                    in
                    mapCurrentAnnotated updateAnnotated imgs
                        |> AllProvided error newDrag config sidePanels classes tools
                        |> (\newState -> Just ( newState, viewer ))

                _ ->
                    Nothing

        _ ->
            Nothing


dragToState : Error -> Config -> SidePanels -> Classes -> Zipper Tool -> AnnotatedZipper -> Pointer.DragState -> State
dragToState error config sidePanels classes tools imgs dragState =
    AllProvided error dragState config sidePanels classes tools imgs


updateMove : Pointer.Msg -> Pointer.DragState -> Viewer -> Maybe ( Pointer.DragState, Viewer )
updateMove pointerMsg dragState viewer =
    case ( pointerMsg, dragState ) of
        ( Pointer.DownAt pos, _ ) ->
            Just ( Pointer.DraggingFrom pos, viewer )

        ( Pointer.MoveAt ( x, y ), Pointer.DraggingFrom ( ox, oy ) ) ->
            Just ( Pointer.DraggingFrom ( x, y ), Viewer.pan ( x - ox, y - oy ) viewer )

        ( Pointer.UpAt _, _ ) ->
            Just ( Pointer.NoDrag, viewer )

        _ ->
            Nothing


removeAnnotation : State -> State
removeAnnotation state =
    case state of
        AllProvided NoError drag config sidePanels classes tools imgs ->
            AllProvided NoError drag config sidePanels classes tools (mapCurrentAnnotated AnnotatedImage.removeAnnotation imgs)

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
            ( ImagesProvided NoError Pointer.NoDrag True (Zipper.init [] firstImage otherImages)
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( first :: files, ConfigProvided _ config visible classes tools ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneLoading setupAnnotatedLoading 0 first

                ( otherImages, otherCmds ) =
                    prepareListLoading setupAnnotatedLoading 1 files

                annotatedImages =
                    Zipper.init [] firstImage otherImages

                sidePanels =
                    { imagesVisible = True
                    , classesVisible = visible
                    }
            in
            ( AllProvided NoError Pointer.NoDrag config sidePanels classes tools annotatedImages
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( files, ImagesProvided _ drag visible previousImages ) ->
            let
                startingId =
                    1 + .id (Zipper.getLast previousImages)

                ( newImages, cmds ) =
                    prepareListLoading setupRemoteLoading startingId files
            in
            ( ImagesProvided NoError drag visible (Zipper.append newImages previousImages)
            , Cmd.batch cmds
            )

        ( files, AllProvided _ drag config sidePanels classes tools previousImages ) ->
            let
                startingId =
                    1 + .id (Zipper.getLast previousImages)

                ( newImages, cmds ) =
                    prepareListLoading setupAnnotatedLoading startingId files

                annotatedImages =
                    Zipper.append newImages previousImages
            in
            ( AllProvided NoError drag config sidePanels classes tools annotatedImages
            , Cmd.batch cmds
            )

        _ ->
            ( state, Cmd.none )


mapCurrentRemote : (RemoteImage -> RemoteImage) -> RemoteZipper -> RemoteZipper
mapCurrentRemote f =
    Zipper.updateC (\c -> { c | remoteImage = f c.remoteImage })


imageLoaded : { id : Int, url : String, width : Int, height : Int } -> Viewer -> State -> ( State, Viewer )
imageLoaded { id, url, width, height } viewer state =
    case state of
        ImagesProvided error drag visible images ->
            let
                newState =
                    Zipper.goTo .id id images
                        |> mapCurrentRemote (\r -> { r | status = RemoteImage.Loaded (Image url width height) })
                        |> Zipper.goTo .id (.id (Zipper.getC images))
                        |> ImagesProvided error drag visible

                newViewer =
                    if .id (Zipper.getC images) == id then
                        Viewer.fitImage 1.0 ( toFloat width, toFloat height ) viewer

                    else
                        viewer
            in
            ( newState, newViewer )

        AllProvided error drag config sidePanels classes tools images ->
            let
                newState =
                    Zipper.goTo .id id images
                        |> mapCurrentAnnotated (\r -> { r | status = AnnotatedImage.Loaded (Image url width height) })
                        |> Zipper.goTo .id (.id (Zipper.getC images))
                        |> AllProvided error drag config sidePanels classes tools

                newViewer =
                    if .id (Zipper.getC images) == id then
                        Viewer.fitImage 1.0 ( toFloat width, toFloat height ) viewer

                    else
                        viewer
            in
            ( newState, newViewer )

        _ ->
            ( state, viewer )



-- Config loading


changeConfig : String -> State -> State
changeConfig configString state =
    case ( state, decodeConfig configString ) of
        -- No error
        ( ImagesProvided _ drag visible images, Ok ( config, classes, tools ) ) ->
            let
                sidePanels =
                    { imagesVisible = visible
                    , classesVisible = True
                    }
            in
            AllProvided NoError drag config sidePanels classes tools (Zipper.mapAll upgradeImage images)

        ( AllProvided _ drag _ sidePanels _ _ images, Ok ( config, classes, tools ) ) ->
            AllProvided NoError drag config sidePanels classes tools (Zipper.mapAll resetImage images)

        ( _, Ok ( config, classes, tools ) ) ->
            ConfigProvided NoError config True classes tools

        -- Error
        ( NothingProvided _, Err configError ) ->
            NothingProvided (ConfigError configError)

        ( ConfigProvided _ config visible classes tools, Err configError ) ->
            ConfigProvided (ConfigError configError) config visible classes tools

        ( ImagesProvided _ drag visible images, Err configError ) ->
            ImagesProvided (ConfigError configError) drag visible images

        ( AllProvided _ drag config sidePanels classes tools images, Err configError ) ->
            AllProvided (ConfigError configError) drag config sidePanels classes tools images


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
        AllProvided _ _ config _ _ _ images ->
            Ports.export <| encode config <| List.map .annotatedImage <| Zipper.getAll images

        _ ->
            Cmd.none


encode : Config -> List AnnotatedImage -> Value
encode config images =
    Encode.object
        [ ( "config", Config.encode config )
        , ( "images", Encode.list AnnotatedImage.encode images )
        ]
