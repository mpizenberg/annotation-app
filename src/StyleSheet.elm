module StyleSheet exposing (..)

import Annotation.Color as Color
import Color
import Style exposing (StyleSheet)
import Style.Color as Color


type Style
    = None
    | Button ButtonState
    | Viewer


type ButtonState
    = Disabled
    | Abled
    | Selected


type ColorVariations
    = FromPalette Int


sheet : StyleSheet Style ColorVariations
sheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style (Button Disabled) <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: Style.opacity 0.2
                :: preventCommon
        , Style.style (Button Abled) <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: Style.hover [ Color.background Color.lightGrey, Style.cursor "pointer" ]
                :: preventCommon
                ++ colorVariations
        , Style.style (Button Selected) <|
            Color.background Color.grey
                :: preventCommon
                ++ colorVariations
        , Style.style Viewer preventCommon
        ]


colorVariations : List (Style.Property class ColorVariations)
colorVariations =
    case Color.palette of
        ( color4, color3, color2, color1, color0 ) ->
            Style.variation (FromPalette 0) [ Color.text color0 ]
                :: Style.variation (FromPalette 1) [ Color.text color1 ]
                :: Style.variation (FromPalette 2) [ Color.text color2 ]
                :: Style.variation (FromPalette 3) [ Color.text color3 ]
                :: Style.variation (FromPalette 4) [ Color.text color4 ]
                :: []


preventCommon : List (Style.Property class variation)
preventCommon =
    Style.prop "touch-action" "none"
        :: noUserSelect


noUserSelect : List (Style.Property class variation)
noUserSelect =
    [ Style.prop "user-select" "none"
    , Style.prop "-webkit-user-select" "none"
    , Style.prop "-moz-user-select" "none"
    , Style.prop "-ms-user-select" "none"
    ]
