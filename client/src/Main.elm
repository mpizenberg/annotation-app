-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (main)

import Browser
import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.Config as Config exposing (Config)
import Data.Image exposing (Image)
import Data.Pointer as Pointer
import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Data.Tool as Tool exposing (Tool)
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Packages.Device as Device exposing (Device)
import Packages.FileSystem as FileSystem exposing (FileSystem)
import Packages.Zipper as Zipper exposing (Zipper)
import Ports
import Viewer exposing (Viewer)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = \_ -> Html.text "Work in progress"
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES #############################################################


type alias Model =
    { state : State
    , viewer : Viewer
    , dragState : Pointer.DragState
    }


type State
    = NothingProvided
    | ConfigProvided Config Classes (Zipper Tool)
    | ImagesProvided (Zipper { id : Int, remoteImage : RemoteImage })
    | AllProvided Config Classes (Zipper Tool) (Zipper { id : Int, annotatedImage : AnnotatedImage })


type alias Classes =
    { selected : Int
    , all : FileSystem
    }


type Msg
    = WindowResizes Device.Size
      -- pointer events
    | PointerMsg Pointer.Msg
      -- select things
    | SelectImage Int
    | SelectTool Int
    | SelectClass Int
      -- files
    | LoadImages (List { name : String, file : Value })
    | ImageLoaded { id : Int, url : String, width : Int, height : Int }
    | LoadConfig Value
    | ConfigLoaded String
    | Export
      -- other actions
    | ZoomMsg ZoomMsg
    | RemoveLatestAnnotation


type ZoomMsg
    = ZoomIn
    | ZoomOut
    | ZoomFit ( Float, Float )


type alias Flags =
    { deviceSize : Device.Size
    , mturkMode : Bool
    , images : List Image
    , config : Maybe String
    }



-- FUNCTIONS #########################################################


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        device =
            Device.classify flags.deviceSize

        viewer =
            Viewer.withSize ( 400, 200 )

        -- layout =
        --     View.pageLayout device
        --
        -- viewParameters =
        --     { device = device
        --     , selectClassMsg = SelectClass
        --     , selectImageMsg = SelectImage
        --     , actionBar =
        --         { size = layout.actionBarSize
        --         , hasAnnotations = False
        --         , mturkMode = flags.mturkMode
        --         , removeLatestAnnotationMsg = RemoveLatestAnnotation
        --         , selectToolMsg = SelectTool
        --         , zoomInMsg = ZoomMsg ZoomIn
        --         , zoomOutMsg = ZoomMsg ZoomOut
        --         , zoomFitMsg = ZoomMsg ZoomFit
        --         , loadConfigMsg = LoadConfig
        --         , loadImagesMsg = LoadImages
        --         , exportMsg = Export
        --         }
        --     , annotationsArea =
        --         { size = layout.viewerSize
        --         , annotationsWithImage = Nothing
        --         , pointerDownMsg = PointerMsg << Pointer.DownAt
        --         , pointerMoveMsg = PointerMsg << Pointer.MoveAt
        --         , pointerUpMsg = PointerMsg << Pointer.UpAt
        --         }
        --     }
        --
        state =
            case flags.config of
                Nothing ->
                    importFlagsImages flags.images

                Just configString ->
                    importFlagsImages flags.images
                        |> changeConfig configString

        model =
            { state = state
            , viewer = viewer
            , dragState = Pointer.NoDrag
            }
    in
    ( model, Cmd.none )


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



-- view : Model -> Html Msg
-- view model =
--     Html.div [ Attributes.style [ ( "height", "100%" ) ] ]
--         [ case model.state of
--             NothingProvided ->
--                 lazy View.viewNothing model.viewParameters
--
--             ConfigProvided config classes tools ->
--                 lazy3 View.viewConfig model.viewParameters tools classes
--
--             ImagesProvided images ->
--                 lazy3 View.viewImages model.viewParameters model.viewer images
--
--             AllProvided config classes tools images ->
--                 View.viewAll model.viewParameters tools model.viewer classes images
--         ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        -- ( WindowResizes size, _ ) ->
        --     let
        --         ( viewParameters, viewerSize ) =
        --             View.updateLayout size model.viewParameters
        --
        --         viewer =
        --             Viewer.setSize viewerSize model.viewer
        --     in
        --     ( { model | viewParameters = viewParameters, viewer = viewer }
        --         |> fitImage
        --         |> updateAnnotationsWithImage
        --     , Cmd.none
        --     )
        ( SelectImage imageId, ImagesProvided remoteImages ) ->
            ( { model | state = ImagesProvided (Zipper.goTo .id imageId remoteImages) }
            , Cmd.none
            )

        ( SelectImage imageId, AllProvided config classes tools images ) ->
            let
                newImages =
                    Zipper.goTo .id imageId images
            in
            { model | state = AllProvided config classes tools newImages }
                |> update (SelectTool (Tool.toId <| Zipper.getC tools))

        ( SelectClass id, ConfigProvided config classes tools ) ->
            ( { model | state = ConfigProvided config { classes | selected = id } tools }
            , Cmd.none
            )

        ( SelectClass id, AllProvided config { selected, all } tools imgs ) ->
            ( { model | state = AllProvided config { selected = id, all = all } tools imgs }
            , Cmd.none
            )

        ( SelectTool toolId, ConfigProvided config classes tools ) ->
            ( { model | state = ConfigProvided config classes (Zipper.goTo Tool.toId toolId tools) }, Cmd.none )

        ( SelectTool toolId, AllProvided config classes tools imgs ) ->
            let
                newTools =
                    Zipper.goTo Tool.toId toolId tools

                newState =
                    AllProvided config classes newTools imgs
            in
            ( { model | state = newState }
            , Cmd.none
            )

        ( PointerMsg pointerMsg, AllProvided config classes tools imgs ) ->
            case Zipper.getC tools of
                Tool.Move ->
                    let
                        ( newViewer, newDragState, hasChanged ) =
                            updateMove pointerMsg model.dragState model.viewer
                    in
                    if hasChanged then
                        ( { model | viewer = newViewer, dragState = newDragState }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    let
                        scaledPointerMsg =
                            case pointerMsg of
                                Pointer.DownAt pos ->
                                    Pointer.DownAt (Viewer.coordinatesAt pos model.viewer)

                                Pointer.MoveAt pos ->
                                    Pointer.MoveAt (Viewer.coordinatesAt pos model.viewer)

                                Pointer.UpAt pos ->
                                    Pointer.UpAt (Viewer.coordinatesAt pos model.viewer)
                    in
                    ( Debug.todo "update pointer", Cmd.none )

        ( ZoomMsg zoomMsg, _ ) ->
            ( updateZoom zoomMsg model, Cmd.none )

        ( RemoveLatestAnnotation, AllProvided config classes tools imgs ) ->
            ( Debug.todo "remove annotation"
            , Cmd.none
            )

        ( LoadImages (first :: files), NothingProvided ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneLoading setupRemoteLoading 0 first

                ( otherImages, otherCmds ) =
                    prepareListLoading setupRemoteLoading 1 files
            in
            ( { model | state = ImagesProvided (Zipper.init [] firstImage otherImages) }
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( LoadImages (first :: files), ConfigProvided config classes tools ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneLoading setupAnnotatedLoading 0 first

                ( otherImages, otherCmds ) =
                    prepareListLoading setupAnnotatedLoading 1 files

                annotatedImages =
                    Zipper.init [] firstImage otherImages
            in
            ( { model | state = AllProvided config classes tools annotatedImages }
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( LoadImages files, ImagesProvided previousImages ) ->
            let
                startingId =
                    1 + (.id << Zipper.getC) (Zipper.goEnd previousImages)

                ( newImages, cmds ) =
                    prepareListLoading setupRemoteLoading startingId files
            in
            ( { model | state = ImagesProvided (Zipper.append newImages previousImages) }
            , Cmd.batch cmds
            )

        ( LoadImages files, AllProvided config classes tools previousImages ) ->
            let
                startingId =
                    1 + (.id << Zipper.getC) (Zipper.goEnd previousImages)

                ( newImages, cmds ) =
                    prepareListLoading setupAnnotatedLoading startingId files

                annotatedImages =
                    Zipper.append newImages previousImages
            in
            ( { model | state = AllProvided config classes tools annotatedImages }
            , Cmd.batch cmds
            )

        ( ImageLoaded { id, url, width, height }, ImagesProvided images ) ->
            Debug.todo "ImageLoaded"

        ( ImageLoaded { id, url, width, height }, AllProvided config classes tools images ) ->
            Debug.todo "ImageLoaded"

        ( LoadConfig jsValue, _ ) ->
            ( model, Ports.loadConfigFile jsValue )

        ( ConfigLoaded configString, _ ) ->
            ( { model | state = changeConfig configString model.state }
            , Cmd.none
            )

        ( Export, AllProvided config _ _ images ) ->
            ( model, Ports.export <| encode config <| List.map .annotatedImage <| Zipper.getAll images )

        _ ->
            ( model, Cmd.none )



-- Config loading


changeConfig : String -> State -> State
changeConfig configString state =
    let
        ( config, classes, tools ) =
            decodeConfig configString
    in
    case state of
        ImagesProvided images ->
            Debug.todo "changeConfig"

        AllProvided _ _ _ images ->
            Debug.todo "changeConfig"

        _ ->
            ConfigProvided config classes tools


decodeConfig : String -> ( Config, Classes, Zipper Tool )
decodeConfig configString =
    let
        config =
            Decode.decodeString Config.decoder configString
                |> Result.withDefault Config.empty

        selected =
            if List.isEmpty config.classes then
                0

            else
                1
    in
    ( config
    , { selected = selected, all = Config.classesFrom config.classes }
    , Config.toolsZipperFromConfig config.tools
        |> Maybe.withDefault (Debug.todo "refactor")
    )



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
    -> ( List loading, List (Cmd Msg) )
prepareListLoading setup startId images =
    List.indexedMap (\id img -> prepareOneLoading setup (startId + id) img) images
        |> List.unzip


prepareOneLoading : (Int -> String -> loading) -> Int -> { name : String, file : Value } -> ( loading, Cmd Msg )
prepareOneLoading setup id { name, file } =
    ( setup id name
    , Ports.loadImageFile { id = id, file = file }
    )



-- Manage zooming


updateZoom : ZoomMsg -> Model -> Model
updateZoom zoomMsg model =
    case zoomMsg of
        ZoomIn ->
            { model | viewer = Viewer.zoomIn model.viewer }

        ZoomOut ->
            { model | viewer = Viewer.zoomOut model.viewer }

        ZoomFit size ->
            { model | viewer = Viewer.fitImage 1.2 size model.viewer }



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



-- Export / save annotations


encode : Config -> List AnnotatedImage -> Value
encode config images =
    Encode.object
        [ ( "config", Config.encode config )
        , ( "images", Encode.list AnnotatedImage.encode images )
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.resizes WindowResizes
        , Ports.imageLoaded ImageLoaded
        , Ports.configLoaded ConfigLoaded
        ]
