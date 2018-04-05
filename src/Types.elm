module Types exposing (..)

import Annotation
import Annotation.Viewer as Viewer exposing (Viewer)
import Class exposing (Classes)
import Control exposing (Control)
import Image exposing (Image)
import Json.Encode as Encode
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Tool exposing (Tool)


type alias Model =
    { device : Device
    , layout : PageLayout
    , config : Annotation.Config
    , classesData : { selectedKey : Int, classes : Classes }
    , imagesData : Zipper ImageData
    , dragState : Annotation.DragState
    , moveThrottleState : Control.State Msg
    , viewer : Viewer
    }


type ImageData
    = EmptyImageData
    | Loading Int String
    | Loaded Int String Image (Zipper Tool.Data)


type alias PageLayout =
    { actionBarSize : ( Float, Float )
    , viewerSize : ( Float, Float )
    }


init : Device.Size -> Model
init =
    initWithConfig Annotation.defaultConfig


initWithConfig : Annotation.Config -> Device.Size -> Model
initWithConfig config size =
    let
        device =
            Device.classify size

        layout =
            pageLayout device

        viewer =
            Viewer.setSize layout.viewerSize Viewer.default
    in
    { device = device
    , layout = layout
    , config = config
    , classesData = { selectedKey = 0, classes = Class.fromConfig config }
    , imagesData = Zipper.init [] EmptyImageData []
    , dragState = Annotation.NoDrag
    , moveThrottleState = Control.initialState
    , viewer = viewer
    }


selectTool : Int -> Zipper Tool.Data -> Zipper Tool.Data
selectTool targetId zipper =
    let
        currentId =
            .id (Zipper.getC zipper)
    in
    if currentId < targetId && Zipper.hasR zipper then
        selectTool targetId (Zipper.goR zipper)
    else if targetId < currentId && Zipper.hasL zipper then
        selectTool targetId (Zipper.goL zipper)
    else
        zipper


pageLayout : Device -> PageLayout
pageLayout device =
    let
        ( barWidth, barHeight ) =
            ( device.size.width |> toFloat
            , deviceActionBarHeight device |> toFloat
            )

        ( viewerWidth, viewerHeight ) =
            ( device.size.width |> toFloat
            , max 0 (toFloat device.size.height - barHeight)
            )
    in
    { actionBarSize = ( barWidth, barHeight )
    , viewerSize = ( viewerWidth, viewerHeight )
    }


deviceActionBarHeight : Device -> Int
deviceActionBarHeight device =
    case ( device.kind, device.orientation ) of
        ( Device.Phone, Device.Portrait ) ->
            device.size.width // 7

        ( Device.Phone, Device.Landscape ) ->
            device.size.width // 13

        ( Device.Tablet, Device.Portrait ) ->
            device.size.width // 10

        ( Device.Tablet, Device.Landscape ) ->
            device.size.width // 16

        _ ->
            min 72 (device.size.width // 16)


type Msg
    = NoOp
    | WindowResizesMsg Device.Size
    | SelectImage Int
    | SelectTool Int
    | SelectClass Int
    | ToggleToolDropdown
    | PointerMsg Annotation.PointerMsg
    | MoveThrottle (Control Msg)
    | ZoomMsg ZoomMsg
    | ClearAnnotations
    | LoadImages (List ( String, Encode.Value ))
    | ImageLoaded ( Int, String, Int, Int )
    | LoadConfigFile Encode.Value
    | ConfigLoaded String


type ZoomMsg
    = ZoomIn
    | ZoomOut
    | ZoomFit
