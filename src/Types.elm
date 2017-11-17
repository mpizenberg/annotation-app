module Types exposing (..)

import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Viewer as Viewer exposing (Viewer)
import Device exposing (Device)
import Tool exposing (Tool)


type alias Model =
    { device : Device
    , tool : Tool
    , toolDropdownOpen : Bool
    , currentDropdownTool : Tool
    , bbox : Maybe BoundingBox
    , dragState : DragState
    , viewer : Viewer
    }


type DragState
    = NoDrag
    | DraggingFrom Position


type alias Position =
    ( Float, Float )


init : Device.Size -> Model
init sizeFlag =
    { device = Device.classify sizeFlag
    , tool = Tool.Move
    , toolDropdownOpen = False
    , currentDropdownTool = Tool.BBox
    , bbox = Nothing
    , dragState = NoDrag
    , viewer = Viewer.default
    }


type Msg
    = NoOp
    | WindowResizesMsg Device.Size
    | SelectTool Tool
    | ToggleToolDropdown
    | PointerMsg PointerMsg


type PointerMsg
    = PointerDownAt Position
    | PointerMoveAt Position
    | PointerUpAt Position
