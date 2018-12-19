-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (main)

import Browser
import Data.Image exposing (Image)
import Data.Pointer as Pointer
import Data.State as State exposing (State)
import Element
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events.Extra.Pointer
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Packages.Device as Device exposing (Device)
import Ports
import View.Main as View
import View.Style as Style
import Viewer exposing (Viewer)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES #############################################################


type alias Model =
    { state : State
    , viewer : Viewer
    }


type Msg
    = WindowResizes Device.Size
      -- pointer events
    | RawPointerDownMsg Value
    | PointerMsg Pointer.Msg
      -- select things
    | SelectImage Int
    | SelectTool Int
    | SelectClass ( Int, Int )
    | ToggleCategory Int
      -- toggle side panels
    | ToggleImagesPanel
    | ToggleClassesPanel
      -- files
    | LoadImages (List { name : String, file : Value })
    | ImageLoaded { id : Int, url : String, width : Int, height : Int }
    | LoadConfig Value
    | ConfigLoaded String
    | Export
      -- other actions
    | ZoomMsg ZoomMsg
    | RemoveAnnotation


msgBuilders : View.Msg Msg
msgBuilders =
    { selectImage = SelectImage
    , classesSidebar =
        { toggleCategory = ToggleCategory
        , selectClass = SelectClass
        }
    , toggleImagesPanel = ToggleImagesPanel
    , toggleClassesPanel = ToggleClassesPanel
    , actionBar =
        { loadImages = LoadImages
        , loadConfig = LoadConfig
        , selectTool = SelectTool
        , removeAnnotation = RemoveAnnotation
        }
    , pointer = PointerMsg
    , rawPointer = RawPointerDownMsg
    }


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
            Viewer.withSize (computeViewerSize flags.deviceSize)

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
                    State.importFlagsImages flags.images

                Just configString ->
                    State.importFlagsImages flags.images
                        |> State.changeConfig configString

        model =
            { state = state
            , viewer = viewer
            }
    in
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        State.NothingProvided error ->
            Element.layout
                Style.base
                (View.nothingProvided msgBuilders error)

        State.ImagesProvided error _ visible remoteZipper ->
            Element.layout
                Style.base
                (View.imagesProvided msgBuilders error visible remoteZipper model.viewer)

        State.ConfigProvided error config visible classes toolsZipper ->
            Element.layout
                Style.base
                (View.configProvided msgBuilders error config visible classes toolsZipper)

        State.AllProvided error _ config sidePanels classes toolsZipper annotatedZipper ->
            Element.layout
                Style.base
                (View.allProvided msgBuilders error config sidePanels classes toolsZipper annotatedZipper model.viewer)



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
    case msg of
        WindowResizes size ->
            ( { model | viewer = windowResizes size model.viewer }, Cmd.none )

        RawPointerDownMsg value ->
            case Decode.decodeValue Html.Events.Extra.Pointer.eventDecoder value of
                Ok event ->
                    let
                        ( newModel, newCmd ) =
                            update (PointerMsg <| Pointer.DownAt event.pointer.offsetPos) model
                    in
                    ( newModel, Cmd.batch [ newCmd, Ports.capture value ] )

                Err _ ->
                    ( model, Cmd.none )

        PointerMsg pointerMsg ->
            State.updateWithPointer pointerMsg model.viewer model.state
                |> Maybe.map (\( state, viewer ) -> ( { model | state = state, viewer = viewer }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        -- Select things
        SelectImage id ->
            let
                ( newState, newViewer ) =
                    State.selectImage id model.viewer model.state
            in
            ( { model | state = newState, viewer = newViewer }, Cmd.none )

        SelectTool id ->
            ( { model | state = State.selectTool id model.state }, Cmd.none )

        ToggleCategory id ->
            ( { model | state = State.toggleCategory id model.state }, Cmd.none )

        SelectClass idPair ->
            ( { model | state = State.selectClass idPair model.state }, Cmd.none )

        -- toggle side panels
        ToggleClassesPanel ->
            ( { model | state = State.toggleClassesPanel model.state }, Cmd.none )

        ToggleImagesPanel ->
            ( { model | state = State.toggleImagesPanel model.state }, Cmd.none )

        -- Zooming
        ZoomMsg zoomMsg ->
            ( updateZoom zoomMsg model, Cmd.none )

        -- Loading images
        LoadImages images ->
            State.loadImages images model.state
                |> Tuple.mapFirst (\state -> { model | state = state })

        ImageLoaded image ->
            let
                ( newState, newViewer ) =
                    State.imageLoaded image model.viewer model.state
            in
            ( { model | state = newState, viewer = newViewer }, Cmd.none )

        -- Loading config
        LoadConfig jsValue ->
            ( model, Ports.loadConfigFile jsValue )

        ConfigLoaded configString ->
            ( { model | state = State.changeConfig configString model.state }, Cmd.none )

        -- Export annotations
        Export ->
            ( model, State.export model.state )

        -- Remove annotation
        RemoveAnnotation ->
            ( { model | state = State.removeAnnotation model.state }, Cmd.none )



-- Manage zooming


windowResizes : Device.Size -> Viewer -> Viewer
windowResizes size viewer =
    { viewer | size = computeViewerSize size }
        |> Viewer.centerAtCoordinates (Viewer.coordinatesAtCenter viewer)


computeViewerSize : Device.Size -> ( Float, Float )
computeViewerSize { width, height } =
    ( toFloat width, toFloat height - 100 )


updateZoom : ZoomMsg -> Model -> Model
updateZoom zoomMsg model =
    case zoomMsg of
        ZoomIn ->
            { model | viewer = Viewer.zoomIn model.viewer }

        ZoomOut ->
            { model | viewer = Viewer.zoomOut model.viewer }

        ZoomFit size ->
            { model | viewer = Viewer.fitImage 1.2 size model.viewer }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.resizes WindowResizes
        , Ports.imageLoaded ImageLoaded
        , Ports.configLoaded ConfigLoaded
        ]
