-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (main)

import Annotation.Viewer as Viewer exposing (Viewer)
import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.Config as Config exposing (Config)
import Data.Pointer as Pointer
import Data.RawImage as RawImage exposing (RawImage)
import Data.Tool as Tool exposing (Tool)
import Dict as Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Image exposing (Image)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Packages.Device as Device exposing (Device)
import Packages.StaticTreeMap as StaticTreeMap exposing (StaticTreeMap)
import Packages.Zipper as Zipper exposing (Zipper)
import Ports
import View.Main as View


main : Program Flags Model Msg
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
    | ZoomFit


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
                , mturkMode = flags.mturkMode
                , removeLatestAnnotationMsg = RemoveLatestAnnotation
                , selectToolMsg = SelectTool
                , zoomInMsg = ZoomMsg ZoomIn
                , zoomOutMsg = ZoomMsg ZoomOut
                , zoomFitMsg = ZoomMsg ZoomFit
                , loadConfigMsg = LoadConfig
                , loadImagesMsg = LoadImages
                , exportMsg = Export
                }
            , annotationsArea =
                { size = layout.viewerSize
                , annotationsWithImage = Nothing
                , pointerDownMsg = PointerMsg << Pointer.DownAt
                , pointerMoveMsg = PointerMsg << Pointer.MoveAt
                , pointerUpMsg = PointerMsg << Pointer.UpAt
                }
            }

        state =
            case flags.config of
                Nothing ->
                    importFlagsImages flags.images

                Just configString ->
                    importFlagsImages flags.images
                        |> changeConfig configString

        model =
            updateAnnotationsWithImage <|
                fitImage <|
                    { viewParameters = viewParameters
                    , state = state
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

        image :: otherImages ->
            let
                toRaw id img =
                    RawImage id img.url (RawImage.Loaded img)

                firstRawImage =
                    toRaw 0 image

                otherRawImages =
                    otherImages
                        |> List.indexedMap (\id img -> toRaw (id + 1) img)
            in
            Zipper.init [] firstRawImage otherRawImages
                |> ImagesProvided


view : Model -> Html Msg
view model =
    Html.div [ Attributes.style [ ( "height", "100%" ) ] ]
        [ case model.state of
            NothingProvided ->
                lazy View.viewNothing model.viewParameters

            ConfigProvided config classes tools ->
                lazy3 View.viewConfig model.viewParameters tools classes

            ImagesProvided images ->
                lazy3 View.viewImages model.viewParameters model.viewer images

            AllProvided config classes tools images ->
                View.viewAll model.viewParameters tools model.viewer classes images
        ]


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
                |> updateAnnotationsWithImage
            , Cmd.none
            )

        ( SelectImage imageId, ImagesProvided rawImages ) ->
            ( { model | state = ImagesProvided (Zipper.goTo .id imageId rawImages) }
                |> fitImage
            , Cmd.none
            )

        ( SelectImage imageId, AllProvided config classes tools images ) ->
            let
                newImages =
                    Zipper.goTo .id imageId images
            in
            { model | state = AllProvided config classes tools newImages }
                |> fitImage
                |> update (SelectTool (.id <| Zipper.getC tools))

        ( SelectClass id, ConfigProvided config classes tools ) ->
            ( { model | state = ConfigProvided config { classes | selected = id } tools }
            , Cmd.none
            )

        ( SelectClass id, AllProvided config { selected, all } tools imgs ) ->
            ( { model | state = AllProvided config { selected = id, all = all } tools imgs }
                |> updateAnnotationsWithImage
            , Cmd.none
            )

        ( SelectTool toolId, ConfigProvided config classes tools ) ->
            ( { model | state = ConfigProvided config classes (Zipper.goTo .id toolId tools) }, Cmd.none )

        ( SelectTool toolId, AllProvided config classes tools imgs ) ->
            let
                newTools =
                    Zipper.goTo .id toolId tools

                newAnnotatedImages =
                    Zipper.updateC (AnnotatedImage.selectTool toolId) imgs

                newState =
                    AllProvided config classes newTools newAnnotatedImages

                hasAnnotations =
                    AnnotatedImage.hasAnnotations (Zipper.getC newAnnotatedImages)

                viewParameters =
                    View.markHasAnnotation hasAnnotations model.viewParameters
            in
            ( { model
                | state = newState
                , viewParameters = viewParameters
              }
                |> updateAnnotationsWithImage
            , Cmd.none
            )

        ( PointerMsg pointerMsg, AllProvided config classes tools imgs ) ->
            case .type_ (Zipper.getC tools) of
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
                                    Pointer.DownAt (Viewer.positionIn model.viewer pos)

                                Pointer.MoveAt pos ->
                                    Pointer.MoveAt (Viewer.positionIn model.viewer pos)

                                Pointer.UpAt pos ->
                                    Pointer.UpAt (Viewer.positionIn model.viewer pos)

                        img =
                            Zipper.getC imgs

                        ( newImg, newDragState, hasAnnotations, hasChanged ) =
                            AnnotatedImage.updateWithPointer model.viewer.zoom classes.selected scaledPointerMsg model.dragState img

                        viewParameters =
                            View.markHasAnnotation hasAnnotations model.viewParameters
                    in
                    if hasChanged then
                        ( { model
                            | dragState = newDragState
                            , state = AllProvided config classes tools (Zipper.setC newImg imgs)
                            , viewParameters = viewParameters
                          }
                            |> updateAnnotationsWithImage
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        ( ZoomMsg zoomMsg, _ ) ->
            ( updateZoom zoomMsg model |> updateAnnotationsWithImage, Cmd.none )

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
                |> updateAnnotationsWithImage
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
            ( { model | state = AllProvided config classes tools annotatedImages }
                |> updateAnnotationsWithImage
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
                |> updateAnnotationsWithImage
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
                    |> (\state -> fitImage { model | state = state })
                    |> updateAnnotationsWithImage
                    |> (\model -> ( model, Cmd.none ))

            else
                Zipper.goTo .id id images
                    |> Zipper.updateC (\img -> { img | status = newStatus })
                    |> Zipper.goTo .id img.id
                    |> AllProvided config classes tools
                    |> (\state -> ( { model | state = state }, Cmd.none ))

        ( LoadConfig jsValue, _ ) ->
            ( model, Ports.loadConfigFile jsValue )

        ( ConfigLoaded configString, _ ) ->
            ( { model | state = changeConfig configString model.state }
                |> updateAnnotationsWithImage
            , Cmd.none
            )

        ( Export, AllProvided config _ _ images ) ->
            ( model, Ports.export <| encode config <| Zipper.getAll images )

        _ ->
            ( model, Cmd.none )



-- Update helper due to limitations of 3 parameters for html lazy


updateAnnotationsWithImage : Model -> Model
updateAnnotationsWithImage model =
    case model.state of
        AllProvided config classes tools images ->
            case .status (Zipper.getC images) of
                AnnotatedImage.Loaded image annotationsZipper ->
                    { model
                        | viewParameters =
                            View.updateAnnotationsWithImage
                                model.viewer.zoom
                                image
                                classes.selected
                                annotationsZipper
                                model.viewParameters
                    }

                _ ->
                    model

        _ ->
            model



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
    , Ports.loadImageFile { id = id, file = file }
    )



-- Manage zooming


updateZoom : ZoomMsg -> Model -> Model
updateZoom zoomMsg model =
    case zoomMsg of
        ZoomIn ->
            { model | viewer = Viewer.setZoomCentered ((4.0 / 3.0) * model.viewer.zoom) model.viewer }

        ZoomOut ->
            { model | viewer = Viewer.setZoomCentered ((3.0 / 4.0) * model.viewer.zoom) model.viewer }

        ZoomFit ->
            fitImage model


fitImage : Model -> Model
fitImage ({ state } as model) =
    case state of
        ImagesProvided images ->
            case .status (Zipper.getC images) of
                RawImage.Loaded img ->
                    { model | viewer = Viewer.fitImage 1.0 img model.viewer }

                _ ->
                    model

        AllProvided _ _ _ images ->
            case .status (Zipper.getC images) of
                AnnotatedImage.Loaded img _ ->
                    { model | viewer = Viewer.fitImage 1.0 img model.viewer }

                _ ->
                    model

        _ ->
            model



-- Pointer movement


updateMove : Pointer.Msg -> Pointer.DragState -> Viewer -> ( Viewer, Pointer.DragState, Bool )
updateMove pointerMsg dragState viewer =
    case ( pointerMsg, dragState ) of
        ( Pointer.DownAt pos, _ ) ->
            ( viewer, Pointer.DraggingFrom pos, True )

        ( Pointer.MoveAt ( x, y ), Pointer.DraggingFrom ( ox, oy ) ) ->
            let
                movement =
                    Viewer.sizeIn viewer ( x - ox, y - oy )
            in
            ( Viewer.grabMove movement viewer, Pointer.DraggingFrom ( x, y ), True )

        ( Pointer.UpAt _, _ ) ->
            ( viewer, Pointer.NoDrag, True )

        _ ->
            ( viewer, dragState, False )



-- Export / save annotations


encode : Config -> List AnnotatedImage -> Value
encode config images =
    let
        annotationsDict =
            Config.annotationsInfoFrom config.annotations
                |> List.indexedMap (\id ann -> ( id + 1, ann ))
                |> Dict.fromList
    in
    Encode.object
        [ ( "config", Config.encode config )
        , ( "images", Encode.list <| List.map (AnnotatedImage.encode annotationsDict) images )
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.resizes WindowResizes
        , Ports.imageLoaded ImageLoaded
        , Ports.configLoaded ConfigLoaded
        ]
