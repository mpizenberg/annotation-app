-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module StyleSheet
    exposing
        ( ButtonState(..)
        , ClassState(..)
        , ColorVariations(..)
        , Style(..)
        , sheet
        )

import Annotation.Color as Color
import Color
import Style exposing (StyleSheet)
import Style.Color as Color
import Style.Font as Font


type Style
    = None
    | Button ButtonState
    | TextButton ButtonState
    | ClassesSidebar
    | ClassItem ClassState
    | Viewer
    | ToolIcon


type ButtonState
    = Disabled
    | Abled
    | Selected


type ClassState
    = SelectedClass
    | NonSelectedClass


type ColorVariations
    = FromPalette Int


sheet : StyleSheet Style ColorVariations
sheet =
    Style.styleSheet
        [ Style.style None []

        -- Action bar buttons
        , Style.style (Button Disabled) <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: Style.opacity 0.2
                :: preventCommon
        , Style.style (Button Abled) <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: Style.hover [ Color.background Color.lightGrey, Style.cursor "pointer" ]
                :: preventCommon
        , Style.style (Button Selected) <|
            Color.background Color.grey
                :: preventCommon
        , Style.style ToolIcon colorVariations

        -- Text buttons
        , Style.style (TextButton Disabled) <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: Style.opacity 0.2
                :: classesCommonStyles
        , Style.style (TextButton Abled) <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: Style.hover [ Color.background Color.lightGrey, Style.cursor "pointer" ]
                :: classesCommonStyles
        , Style.style (TextButton Selected) <|
            Color.background Color.grey
                :: classesCommonStyles

        -- Viewer
        , Style.style Viewer preventCommon

        -- Classes sidebar
        , Style.style ClassesSidebar <|
            Color.background (Color.rgba 255 255 255 0.8)
                :: preventCommon
        , Style.style (ClassItem NonSelectedClass) <|
            Style.hover [ Color.background Color.lightGrey, Style.cursor "pointer" ]
                :: classesCommonStyles
        , Style.style (ClassItem SelectedClass) <|
            Color.background Color.grey
                :: classesCommonStyles
        ]


classesCommonStyles : List (Style.Property class var)
classesCommonStyles =
    Font.size 30
        :: preventCommon


colorVariations : List (Style.Property class ColorVariations)
colorVariations =
    case Color.palette of
        ( color4, color3, color2, color1, color0 ) ->
            Style.variation (FromPalette 0) [ Color.text Color.black ]
                :: Style.variation (FromPalette 1) [ Color.text color0 ]
                :: Style.variation (FromPalette 2) [ Color.text color1 ]
                :: Style.variation (FromPalette 3) [ Color.text color2 ]
                :: Style.variation (FromPalette 4) [ Color.text color3 ]
                :: Style.variation (FromPalette 5) [ Color.text color4 ]
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
