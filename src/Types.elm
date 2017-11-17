module Types exposing (..)

import Device exposing (Device)
import Tool exposing (Tool)


type alias Model =
    { device : Device
    , tool : Tool
    , toolDropdownOpen : Bool
    , currentDropdownTool : Tool
    }


type Msg
    = NoOp
    | WindowResizesMsg Device.Size
    | SelectTool Tool
    | ToggleToolDropdown
