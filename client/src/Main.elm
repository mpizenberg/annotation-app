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
import Json.Encode as Encode exposing (Value)
import Packages.Device as Device exposing (Device)
import Ports
import View.Main as View
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
    | PointerMsg Pointer.Msg
      -- select things
    | SelectImage Int
    | SelectTool Int
    | ToggleCategory Int
    | SelectClass ( Int, Int )
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
    { actionBar =
        { loadImages = LoadImages
        }
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
            Element.layout [] (View.nothingProvided msgBuilders error)

        _ ->
            Debug.todo "view"



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

        PointerMsg pointerMsg ->
            State.updateWithPointer pointerMsg model.viewer model.state
                |> Maybe.map (\( state, viewer ) -> ( { model | state = state, viewer = viewer }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        -- Select things
        SelectImage id ->
            ( { model | state = State.selectImage id model.state }, Cmd.none )

        SelectTool id ->
            ( { model | state = State.selectTool id model.state }, Cmd.none )

        ToggleCategory id ->
            ( { model | state = State.toggleCategory id model.state }, Cmd.none )

        SelectClass idPair ->
            ( { model | state = State.selectClass idPair model.state }, Cmd.none )

        -- Zooming
        ZoomMsg zoomMsg ->
            ( updateZoom zoomMsg model, Cmd.none )

        -- Loading images
        LoadImages images ->
            State.loadImages images model.state
                |> Tuple.mapFirst (\state -> { model | state = state })

        ImageLoaded image ->
            ( { model | state = State.imageLoaded image model.state }, Cmd.none )

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
windowResizes { width, height } viewer =
    let
        center =
            Viewer.coordinatesAtCenter viewer
    in
    Viewer.centerAtCoordinates center { viewer | size = ( toFloat width, toFloat height ) }


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
