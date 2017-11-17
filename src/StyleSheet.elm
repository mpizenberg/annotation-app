module StyleSheet exposing (..)

import Color
import Style exposing (StyleSheet)
import Style.Color as Color


type Style
    = None
    | CurrentTool
    | Button Bool
    | Viewer


sheet : StyleSheet Style variation
sheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style CurrentTool <|
            [ Color.background Color.grey
            , Style.prop "touch-action" "none"
            ]
                ++ noUserSelect
        , Style.style (Button False) <|
            [ Style.hover [ Color.background Color.lightGrey, Style.cursor "pointer" ]
            , Style.prop "touch-action" "none"
            ]
                ++ noUserSelect
        , Style.style (Button True) <|
            [ Color.text Color.lightGrey
            , Style.prop "touch-action" "none"
            ]
                ++ noUserSelect
        , Style.style Viewer <|
            Style.prop "touch-action" "none"
                :: noUserSelect
        ]


noUserSelect : List (Style.Property class variation)
noUserSelect =
    [ Style.prop "user-select" "none"
    , Style.prop "-webkit-user-select" "none"
    , Style.prop "-moz-user-select" "none"
    , Style.prop "-ms-user-select" "none"
    ]
