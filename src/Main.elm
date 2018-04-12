-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (main)

import Annotation.Viewer as Viewer exposing (Viewer)
import Control exposing (Control)
import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.Config as Config exposing (Config)
import Data.Pointer as Pointer
import Data.RawImage as RawImage exposing (RawImage)
import Data.Tool as Tool exposing (Tool)
import Html exposing (Html)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Image exposing (Image)
import Json.Decode as Decode exposing (Decoder, Value)
import Packages.Device as Device exposing (Device)
import Packages.StaticTreeMap as StaticTreeMap exposing (StaticTreeMap)
import Packages.Zipper as Zipper exposing (Zipper)
import Ports
import View.Main as View


main : Program Device.Size Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = lazy view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES #############################################################


type alias Model =
    { viewParameters : View.Parameters Msg
    , state : State
    , viewer : Viewer
    , dragState : Pointer.DragState
    , moveThrottleState : Control.State Msg
    }


type State
    = NothingProvided
    | ConfigProvided Config Classes (Zipper Tool)
    | ImagesProvided (Zipper RawImage)
    | AllProvided Config Classes (Zipper Tool) (Zipper AnnotatedImage)


type alias Classes =
    { selected : Int
    , all : StaticTreeMap String
    }


type Msg
    = WindowResizes Device.Size
      -- pointer events
    | PointerMsg Pointer.Msg
    | MoveThrottle (Control Msg)
      -- select things
    | SelectImage Int
    | SelectTool Int
    | SelectClass Int
      -- files
    | LoadImages (List { name : String, file : Value })
    | ImageLoaded { id : Int, url : String, width : Int, height : Int }
    | LoadConfig Value
    | ConfigLoaded String
      -- other actions
    | ZoomMsg ZoomMsg
    | RemoveLatestAnnotation


type ZoomMsg
    = ZoomIn
    | ZoomOut
    | ZoomFit



-- FUNCTIONS #########################################################


init : Device.Size -> ( Model, Cmd Msg )
init sizeFlag =
    let
        device =
            Device.classify sizeFlag

        layout =
            View.pageLayout device

        viewer =
            Viewer.setSize layout.viewerSize Viewer.default

        viewParameters =
            { device = device
            , selectClassMsg = SelectClass
            , selectImageMsg = SelectImage
            , actionBar =
                { size = layout.actionBarSize
                , hasAnnotations = False
                , hasImage = False
                , removeLatestAnnotationMsg = RemoveLatestAnnotation
                , selectToolMsg = SelectTool
                , zoomInMsg = ZoomMsg ZoomIn
                , zoomOutMsg = ZoomMsg ZoomOut
                , zoomFitMsg = ZoomMsg ZoomFit
                , loadConfigMsg = LoadConfig
                , loadImagesMsg = LoadImages
                }
            , annotationsArea =
                { size = layout.viewerSize
                , pointerDownMsg = PointerMsg << Pointer.DownAt
                , pointerMoveMsg = PointerMsg << Pointer.MoveAt
                , pointerUpMsg = PointerMsg << Pointer.UpAt
                , throttleMsg = MoveThrottle
                }
            }

        model =
            { viewParameters = viewParameters
            , state = NothingProvided
            , viewer = viewer
            , dragState = Pointer.NoDrag
            , moveThrottleState = Control.initialState
            }
    in
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        NothingProvided ->
            View.viewNothing model.viewParameters

        ConfigProvided config classes tools ->
            View.viewConfig model.viewParameters tools classes

        ImagesProvided images ->
            View.viewImages model.viewParameters model.viewer images

        AllProvided config classes tools images ->
            View.viewAll model.viewParameters tools model.viewer classes images


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( WindowResizes size, _ ) ->
            let
                ( viewParameters, viewerSize ) =
                    View.updateLayout size model.viewParameters

                viewer =
                    Viewer.setSize viewerSize model.viewer
            in
            ( { model | viewParameters = viewParameters, viewer = viewer }
                |> fitImage
            , Cmd.none
            )

        ( SelectImage imageId, ImagesProvided rawImages ) ->
            ( { model | state = ImagesProvided (Zipper.goTo .id imageId rawImages) }
                |> fitImage
            , Cmd.none
            )

        ( SelectImage imageId, AllProvided co cl t images ) ->
            ( { model | state = AllProvided co cl t (Zipper.goTo .id imageId images) }
                |> fitImage
            , Cmd.none
            )

        ( SelectClass id, ConfigProvided config classes tools ) ->
            let
                _ =
                    Debug.log "msg" msg
            in
            ( { model | state = ConfigProvided config { classes | selected = id } tools }
            , Cmd.none
            )

        ( SelectClass id, AllProvided config { selected, all } tools imgs ) ->
            ( { model
                | state = AllProvided config { selected = id, all = all } tools imgs
              }
            , Cmd.none
            )

        ( SelectTool toolId, ConfigProvided config classes tools ) ->
            ( { model | state = ConfigProvided config classes (Zipper.goTo .id toolId tools) }, Cmd.none )

        ( SelectTool toolId, AllProvided config classes tools imgs ) ->
            ( { model
                | state =
                    AllProvided config
                        classes
                        (Zipper.goTo .id toolId tools)
                        (Zipper.updateC (AnnotatedImage.selectTool toolId) imgs)
              }
            , Cmd.none
            )

        ( PointerMsg pointerMsg, AllProvided config classes tools imgs ) ->
            case .type_ (Zipper.getC tools) of
                Tool.Move ->
                    let
                        ( newViewer, newDragState ) =
                            updateMove pointerMsg model.dragState model.viewer
                    in
                    ( { model | viewer = newViewer, dragState = newDragState }, Cmd.none )

                _ ->
                    let
                        scaledPointerMsg =
                            case pointerMsg of
                                Pointer.DownAt pos ->
                                    Pointer.DownAt (Viewer.positionIn model.viewer pos)

                                Pointer.MoveAt pos ->
                                    Pointer.MoveAt (Viewer.positionIn model.viewer pos)

                                Pointer.UpAt pos ->
                                    Pointer.UpAt (Viewer.positionIn model.viewer pos)

                        img =
                            Zipper.getC imgs

                        ( newImg, newDragState, hasAnnotations ) =
                            AnnotatedImage.updateWithPointer model.viewer.zoom classes.selected scaledPointerMsg model.dragState img

                        viewParameters =
                            View.markHasAnnotation hasAnnotations model.viewParameters
                    in
                    ( { model
                        | dragState = newDragState
                        , state = AllProvided config classes tools (Zipper.setC newImg imgs)
                        , viewParameters = viewParameters
                      }
                    , Cmd.none
                    )

        ( MoveThrottle throttleMsg, AllProvided _ _ _ _ ) ->
            Control.update (\s -> { model | moveThrottleState = s }) model.moveThrottleState throttleMsg

        ( ZoomMsg zoomMsg, _ ) ->
            ( updateZoom zoomMsg model, Cmd.none )

        ( RemoveLatestAnnotation, AllProvided config classes tools imgs ) ->
            let
                newZipper =
                    Zipper.updateC AnnotatedImage.removeLatestAnnotation imgs

                newState =
                    AllProvided config classes tools newZipper

                hasAnnotations =
                    AnnotatedImage.hasAnnotations (Zipper.getC newZipper)

                viewParameters =
                    View.markHasAnnotation hasAnnotations model.viewParameters
            in
            ( { model | state = newState, viewParameters = viewParameters }
            , Cmd.none
            )

        ( LoadImages (f :: files), NothingProvided ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneRawLoading 0 f

                ( otherImages, otherCmds ) =
                    prepareRawLoading 1 files
            in
            ( { model | state = ImagesProvided (Zipper.init [] firstImage otherImages) }
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( LoadImages (f :: files), ConfigProvided config classes tools ) ->
            let
                ( firstImage, firstCmd ) =
                    prepareOneRawLoading 0 f

                ( otherImages, otherCmds ) =
                    prepareRawLoading 1 files

                annotatedImages =
                    Zipper.init []
                        (AnnotatedImage.fromRaw tools firstImage)
                        (List.map (AnnotatedImage.fromRaw tools) otherImages)
            in
            ( { model
                | state = AllProvided config classes tools annotatedImages
                , viewParameters = View.markHasImage model.viewParameters
              }
            , Cmd.batch (firstCmd :: otherCmds)
            )

        ( LoadImages files, ImagesProvided previousImages ) ->
            let
                startingId =
                    1 + (.id << Zipper.getC) (Zipper.goEnd previousImages)

                ( newImages, cmds ) =
                    prepareRawLoading startingId files
            in
            ( { model | state = ImagesProvided (Zipper.append newImages previousImages) }
            , Cmd.batch cmds
            )

        ( LoadImages files, AllProvided config classes tools previousImages ) ->
            let
                startingId =
                    1 + (.id << Zipper.getC) (Zipper.goEnd previousImages)

                ( newImages, cmds ) =
                    prepareRawLoading startingId files

                newAnnotatedImages =
                    List.map (AnnotatedImage.fromRaw tools) newImages
            in
            ( { model | state = AllProvided config classes tools (Zipper.append newAnnotatedImages previousImages) }
            , Cmd.batch cmds
            )

        ( ImageLoaded { id, url, width, height }, ImagesProvided images ) ->
            let
                img =
                    Zipper.getC images

                newStatus =
                    RawImage.Loaded (Image url width height)
            in
            if id == img.id then
                Zipper.setC { img | status = newStatus } images
                    |> ImagesProvided
                    |> (\state -> ( fitImage { model | state = state }, Cmd.none ))
            else
                Zipper.goTo .id id images
                    |> Zipper.updateC (\img -> { img | status = newStatus })
                    |> Zipper.goTo .id img.id
                    |> ImagesProvided
                    |> (\state -> ( { model | state = state }, Cmd.none ))

        ( ImageLoaded { id, url, width, height }, AllProvided config classes tools images ) ->
            let
                img =
                    Zipper.getC images

                newStatus =
                    AnnotatedImage.Loaded (Image url width height)
                        (AnnotatedImage.annotationsFromTools tools)
            in
            if id == img.id then
                Zipper.setC { img | status = newStatus } images
                    |> AllProvided config classes tools
                    |> (\state -> ( fitImage { model | state = state }, Cmd.none ))
            else
                Zipper.goTo .id id images
                    |> Zipper.updateC (\img -> { img | status = newStatus })
                    |> Zipper.goTo .id img.id
                    |> AllProvided config classes tools
                    |> (\state -> ( { model | state = state }, Cmd.none ))

        ( LoadConfig jsValue, _ ) ->
            ( model, Ports.loadConfigFile jsValue )

        ( ConfigLoaded configString, _ ) ->
            ( { model
                | state = changeConfig configString model.state
                , viewParameters = View.markHasImage model.viewParameters
              }
            , Cmd.none
            )

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
            Zipper.mapAll (AnnotatedImage.fromRaw tools) images
                |> AllProvided config classes tools

        AllProvided _ _ _ images ->
            Zipper.mapAll (AnnotatedImage.resetWithTools tools) images
                |> AllProvided config classes tools

        _ ->
            ConfigProvided config classes tools


decodeConfig : String -> ( Config, { selected : Int, all : StaticTreeMap String }, Zipper Tool )
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
    , Config.toolsFrom config.annotations
    )



-- Images loading


prepareRawLoading : Int -> List { name : String, file : Value } -> ( List RawImage, List (Cmd Msg) )
prepareRawLoading startId images =
    let
        nbImages =
            List.length images

        ids =
            List.range startId (startId + nbImages)
    in
    List.map2 prepareOneRawLoading ids images
        |> List.unzip


prepareOneRawLoading : Int -> { name : String, file : Value } -> ( RawImage, Cmd Msg )
prepareOneRawLoading id { name, file } =
    ( { id = id, name = name, status = RawImage.Loading }
    , Ports.loadImageFile ( id, file )
    )



-- Manage zooming


updateZoom : ZoomMsg -> Model -> Model
updateZoom zoomMsg model =
    case zoomMsg of
        ZoomIn ->
            { model | viewer = Viewer.setZoomCentered (1.5625 * model.viewer.zoom) model.viewer }

        ZoomOut ->
            { model | viewer = Viewer.setZoomCentered (0.64 * model.viewer.zoom) model.viewer }

        ZoomFit ->
            fitImage model


fitImage : Model -> Model
fitImage ({ state } as model) =
    case state of
        ImagesProvided images ->
            case .status (Zipper.getC images) of
                RawImage.Loaded img ->
                    { model | viewer = Viewer.fitImage 0.8 img model.viewer }

                _ ->
                    model

        AllProvided _ _ _ images ->
            case .status (Zipper.getC images) of
                AnnotatedImage.Loaded img _ ->
                    { model | viewer = Viewer.fitImage 0.8 img model.viewer }

                _ ->
                    model

        _ ->
            model



-- Pointer movement


updateMove : Pointer.Msg -> Pointer.DragState -> Viewer -> ( Viewer, Pointer.DragState )
updateMove pointerMsg dragState viewer =
    case ( pointerMsg, dragState ) of
        ( Pointer.DownAt pos, _ ) ->
            ( viewer, Pointer.DraggingFrom pos )

        ( Pointer.MoveAt ( x, y ), Pointer.DraggingFrom ( ox, oy ) ) ->
            let
                movement =
                    Viewer.sizeIn viewer ( x - ox, y - oy )
            in
            ( Viewer.grabMove movement viewer, Pointer.DraggingFrom ( x, y ) )

        ( Pointer.UpAt _, _ ) ->
            ( viewer, Pointer.NoDrag )

        _ ->
            ( viewer, dragState )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.resizes WindowResizes
        , Ports.imageLoaded ImageLoaded
        , Ports.configLoaded ConfigLoaded
        ]
