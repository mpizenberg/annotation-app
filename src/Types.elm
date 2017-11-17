module Types exposing (..)

import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Viewer as Viewer exposing (Viewer)
import Control exposing (Control)
import Device exposing (Device)
import Tool exposing (Tool)


type alias Model =
    { device : Device
    , layout : PageLayout
    , tool : Tool
    , toolDropdownOpen : Bool
    , currentDropdownTool : Tool
    , bbox : Maybe BoundingBox
    , dragState : DragState
    , moveThrottleState : Control.State Msg
    , viewer : Viewer
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
    , tool = Tool.Move
    , toolDropdownOpen = False
    , currentDropdownTool = Tool.BBox
    , bbox = Nothing
    , dragState = NoDrag
    , moveThrottleState = Control.initialState
    , viewer = viewer
    }


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
    | SelectTool Tool
    | ToggleToolDropdown
    | PointerMsg PointerMsg
    | MoveThrottle (Control Msg)
    | ZoomMsg ZoomMsg


type PointerMsg
    = PointerDownAt Position
    | PointerMoveAt Position
    | PointerUpAt Position


type ZoomMsg
    = ZoomIn
    | ZoomOut
    | ZoomFit
