-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module StyleSheet exposing
    ( ButtonState(..)
    , ClassState(..)
    , Style(..)
    , sheet
    )

import Future.Color as Color exposing (Color)
import Future.StyleColor as StyleColor
import Style exposing (StyleSheet)
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


sheet : StyleSheet Style variations
sheet =
    Style.styleSheet
        [ Style.style None []

        -- Action bar buttons
        , Style.style (Button Disabled) <|
            StyleColor.background (Color.rgba 255 255 255 0.8)
                :: Style.opacity 0.2
                :: preventCommon
        , Style.style (Button Abled) <|
            StyleColor.background (Color.rgba 255 255 255 0.8)
                :: Style.hover [ StyleColor.background Color.lightGrey, Style.cursor "pointer" ]
                :: preventCommon
        , Style.style (Button Selected) <|
            StyleColor.background Color.grey
                :: preventCommon
        , Style.style ToolIcon [ StyleColor.text Color.black ]

        -- Text buttons
        , Style.style (TextButton Disabled) <|
            StyleColor.background (Color.rgba 255 255 255 0.8)
                :: Style.opacity 0.2
                :: classesCommonStyles
        , Style.style (TextButton Abled) <|
            StyleColor.background (Color.rgba 255 255 255 0.8)
                :: Style.hover [ StyleColor.background Color.lightGrey, Style.cursor "pointer" ]
                :: classesCommonStyles
        , Style.style (TextButton Selected) <|
            StyleColor.background Color.grey
                :: classesCommonStyles

        -- Viewer
        , Style.style Viewer preventCommon

        -- Classes sidebar
        , Style.style ClassesSidebar <|
            StyleColor.background (Color.rgba 255 255 255 0.8)
                :: preventCommon
        , Style.style (ClassItem NonSelectedClass) <|
            Style.hover [ StyleColor.background Color.lightGrey, Style.cursor "pointer" ]
                :: classesCommonStyles
        , Style.style (ClassItem SelectedClass) <|
            StyleColor.background Color.grey
                :: classesCommonStyles
        ]


classesCommonStyles : List (Style.Property class var)
classesCommonStyles =
    Font.size 30
        :: preventCommon


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
