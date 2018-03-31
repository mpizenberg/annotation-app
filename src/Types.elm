module Types exposing (..)

import Annotation
import Annotation.Viewer as Viewer exposing (Viewer)
import Control exposing (Control)
import Image exposing (Image)
import Json.Encode as Encode
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Tool exposing (Tool)


type alias Model =
    { device : Device
    , layout : PageLayout
    , toolsData : Zipper Tool.Data
    , dragState : DragState
    , moveThrottleState : Control.State Msg
    , viewer : Viewer
    , image : Maybe Image
    }


type alias PageLayout =
    { actionBarSize : ( Float, Float )
    , viewerSize : ( Float, Float )
    }


type DragState
    = NoDrag
    | DraggingFrom Position


type alias Position =
    ( Float, Float )


init : Device.Size -> Model
init sizeFlag =
    let
        device =
            Device.classify sizeFlag

        layout =
            pageLayout device

        viewer =
            Viewer.setSize layout.viewerSize Viewer.default
    in
    { device = device
    , layout = layout
    , toolsData = Tool.fromConfig Annotation.emptyConfig
    , dragState = NoDrag
    , moveThrottleState = Control.initialState
    , viewer = viewer
    , image = Nothing
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
    | SelectTool Int
    | ToggleToolDropdown
    | PointerMsg PointerMsg
    | MoveThrottle (Control Msg)
    | ZoomMsg ZoomMsg
    | ClearAnnotations
    | LoadImageFile Encode.Value
    | ImageLoaded ( String, Int, Int )


type PointerMsg
    = PointerDownAt Position
    | PointerMoveAt Position
    | PointerUpAt Position


type ZoomMsg
    = ZoomIn
    | ZoomOut
    | ZoomFit
