module Data.State exposing
    ( State(..), Classes
    , importFlagsImages
    , updateWithPointer
    , selectClass, selectImage, selectTool
    , loadImages, imageLoaded
    , changeConfig
    , export
    , removeAnnotation
    )

{-| Module doc

@docs State, Classes

@docs importFlagsImages

@docs updateWithPointer

@docs selectClass, selectImage, selectTool

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


type State
    = NothingProvided
    | ConfigProvided Config Classes (Zipper Tool)
    | ImagesProvided (Zipper { id : Int, remoteImage : RemoteImage })
    | AllProvided Config Classes (Zipper Tool) (Zipper { id : Int, annotatedImage : AnnotatedImage })


type alias Classes =
    { selected : Int
    , all : FileSystem
    }


importFlagsImages : List Image -> State
importFlagsImages images =
    case images of
        [] ->
            NothingProvided

        firstImage :: otherImages ->
            let
                otherRemoteImagesWithId =
                    List.indexedMap (\id img -> remoteImageWithId (id + 1) img) otherImages
            in
            Zipper.init [] (remoteImageWithId 0 firstImage) otherRemoteImagesWithId
                |> ImagesProvided


remoteImageWithId : Int -> Image -> { id : Int, remoteImage : RemoteImage }
remoteImageWithId id image =
    { id = id
    , remoteImage = RemoteImage image.url (RemoteImage.Loaded image)
    }



-- UPDATE ############################################################


selectImage : Int -> State -> State
selectImage id state =
    -- ( SelectImage imageId, ImagesProvided remoteImages ) ->
    --     ( { model | state = ImagesProvided (Zipper.goTo .id imageId remoteImages) }
    --     , Cmd.none
    --     )
    --
    -- ( SelectImage imageId, AllProvided config classes tools images ) ->
    --     let
    --         newImages =
    --             Zipper.goTo .id imageId images
    --     in
    --     { model | state = AllProvided config classes tools newImages }
    --         |> update (SelectTool (Tool.toId <| Zipper.getC tools))
    Debug.todo "selectImage"


selectClass : Int -> State -> State
selectClass id state =
    -- ( SelectClass id, ConfigProvided config classes tools ) ->
    --     ( { model | state = ConfigProvided config { classes | selected = id } tools }
    --     , Cmd.none
    --     )
    --
    -- ( SelectClass id, AllProvided config { selected, all } tools imgs ) ->
    --     ( { model | state = AllProvided config { selected = id, all = all } tools imgs }
    --     , Cmd.none
    --     )
    Debug.todo "selectClass"


selectTool : Int -> State -> State
selectTool id state =
    -- ( SelectTool toolId, ConfigProvided config classes tools ) ->
    --     ( { model | state = ConfigProvided config classes (Zipper.goTo Tool.toId toolId tools) }, Cmd.none )
    --
    -- ( SelectTool toolId, AllProvided config classes tools imgs ) ->
    --     let
    --         newTools =
    --             Zipper.goTo Tool.toId toolId tools
    --
    --         newState =
    --             AllProvided config classes newTools imgs
    --     in
    --     ( { model | state = newState }
    --     , Cmd.none
    --     )
    Debug.todo "selectTool"


updateWithPointer : Pointer.Msg -> State -> State
updateWithPointer pointerMsg state =
    -- ( PointerMsg pointerMsg, AllProvided config classes tools imgs ) ->
    --     case Zipper.getC tools of
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
    -- ( RemoveAnnotation, AllProvided config classes tools imgs ) ->
    --     let
    --         currentImage =
    --             Zipper.getC imgs
    --
    --         annotatedImage =
    --             AnnotatedImage.removeAnnotation currentImage.annotatedImage
    --
    --         newZipper =
    --             Zipper.setC { currentImage | annotatedImage = annotatedImage } imgs
    --     in
    --     ( { model | state = AllProvided config classes tools newZipper }
    --     , Cmd.none
    --     )
    Debug.todo "removeAnnotation"


loadImages : List { name : String, file : Value } -> State -> ( State, Cmd msg )
loadImages images state =
    -- ( LoadImages (first :: files), NothingProvided ) ->
    --     let
    --         ( firstImage, firstCmd ) =
    --             prepareOneLoading setupRemoteLoading 0 first
    --
    --         ( otherImages, otherCmds ) =
    --             prepareListLoading setupRemoteLoading 1 files
    --     in
    --     ( { model | state = ImagesProvided (Zipper.init [] firstImage otherImages) }
    --     , Cmd.batch (firstCmd :: otherCmds)
    --     )
    --
    -- ( LoadImages (first :: files), ConfigProvided config classes tools ) ->
    --     let
    --         ( firstImage, firstCmd ) =
    --             prepareOneLoading setupAnnotatedLoading 0 first
    --
    --         ( otherImages, otherCmds ) =
    --             prepareListLoading setupAnnotatedLoading 1 files
    --
    --         annotatedImages =
    --             Zipper.init [] firstImage otherImages
    --     in
    --     ( { model | state = AllProvided config classes tools annotatedImages }
    --     , Cmd.batch (firstCmd :: otherCmds)
    --     )
    --
    -- ( LoadImages files, ImagesProvided previousImages ) ->
    --     let
    --         startingId =
    --             1 + (.id << Zipper.getC) (Zipper.goEnd previousImages)
    --
    --         ( newImages, cmds ) =
    --             prepareListLoading setupRemoteLoading startingId files
    --     in
    --     ( { model | state = ImagesProvided (Zipper.append newImages previousImages) }
    --     , Cmd.batch cmds
    --     )
    --
    -- ( LoadImages files, AllProvided config classes tools previousImages ) ->
    --     let
    --         startingId =
    --             1 + (.id << Zipper.getC) (Zipper.goEnd previousImages)
    --
    --         ( newImages, cmds ) =
    --             prepareListLoading setupAnnotatedLoading startingId files
    --
    --         annotatedImages =
    --             Zipper.append newImages previousImages
    --     in
    --     ( { model | state = AllProvided config classes tools annotatedImages }
    --     , Cmd.batch cmds
    --     )
    Debug.todo "loadImages"


imageLoaded : { id : Int, url : String, width : Int, height : Int } -> State -> State
imageLoaded { id, url, width, height } state =
    -- ( ImageLoaded { id, url, width, height }, ImagesProvided images ) ->
    --     let
    --         movedZipper =
    --             Zipper.goTo .id id images
    --
    --         { name } =
    --             .remoteImage (Zipper.getC movedZipper)
    --
    --         loaded =
    --             { name = name, status = RemoteImage.Loaded (Image url width height) }
    --
    --         newZipper =
    --             Zipper.setC { id = id, remoteImage = loaded } movedZipper
    --                 |> Zipper.goTo .id (.id (Zipper.getC images))
    --     in
    --     ( { model | state = ImagesProvided newZipper }
    --     , Cmd.none
    --     )
    --
    -- ( ImageLoaded { id, url, width, height }, AllProvided config classes tools images ) ->
    --     let
    --         movedZipper =
    --             Zipper.goTo .id id images
    --
    --         { name } =
    --             .annotatedImage (Zipper.getC movedZipper)
    --
    --         loaded =
    --             { name = name, status = AnnotatedImage.Loaded (Image url width height) }
    --
    --         newZipper =
    --             Zipper.setC { id = id, annotatedImage = loaded } movedZipper
    --                 |> Zipper.goTo .id (.id (Zipper.getC images))
    --     in
    --     ( { model | state = AllProvided config classes tools newZipper }
    --     , Cmd.none
    --     )
    Debug.todo "imageLoaded"



-- Config loading


changeConfig : String -> State -> State
changeConfig configString state =
    case ( state, decodeConfig configString ) of
        ( ImagesProvided images, Ok ( config, classes, tools ) ) ->
            AllProvided config classes tools (Zipper.mapAll upgradeImage images)

        ( AllProvided _ _ _ images, Ok ( config, classes, tools ) ) ->
            AllProvided config classes tools (Zipper.mapAll resetImage images)

        ( _, Ok ( config, classes, tools ) ) ->
            ConfigProvided config classes tools

        ( _, Err _ ) ->
            Debug.todo "handle error in state"


decodeConfig : String -> Result Config.Error ( Config, Classes, Zipper Tool )
decodeConfig configString =
    case Decode.decodeString Config.decoder configString of
        Ok config ->
            let
                selected =
                    if List.isEmpty config.classes then
                        0

                    else
                        1
            in
            case config.tools of
                [] ->
                    Err Config.IncorrectTools

                firstTool :: otherTools ->
                    Ok
                        ( config
                        , { selected = selected, all = Config.classesFrom config.classes }
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
        AllProvided config _ _ images ->
            Ports.export <| encode config <| List.map .annotatedImage <| Zipper.getAll images

        _ ->
            Cmd.none


encode : Config -> List AnnotatedImage -> Value
encode config images =
    Encode.object
        [ ( "config", Config.encode config )
        , ( "images", Encode.list AnnotatedImage.encode images )
        ]
