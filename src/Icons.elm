-- Module containing svg icons generated from 1602/elm-feather-icons
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Icons
    exposing
        ( boundingBox
        , contour
        , defaultAttributes
        , download
        , image
        , maximize2
        , moreVertical
        , move
        , octagon
        , outline
        , rotateCcw
        , save
        , sized
        , trash2
        , zoomFit
        , zoomIn
        , zoomOut
        )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


sized : Float -> List (Svg msg) -> Html msg
sized size =
    svg (width (toString size) :: height (toString size) :: defaultAttributes)


defaultAttributes : List (Svg.Attribute msg)
defaultAttributes =
    [ fill "none"
    , stroke "currentColor"
    , strokeLinecap "round"
    , strokeLinejoin "round"
    , strokeWidth "2"
    , viewBox "0 0 24 24"
    ]


download : List (Svg msg)
download =
    [ Svg.path [ d "M3 17v3a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-3" ] []
    , Svg.polyline [ points "8 12 12 16 16 12" ] []
    , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "16" ] []
    ]


image : List (Svg msg)
image =
    [ Svg.rect [ x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
    , Svg.circle [ cx "8.5", cy "8.5", r "1.5" ] []
    , Svg.polyline [ points "21 15 16 10 5 21" ] []
    ]


maximize2 : List (Svg msg)
maximize2 =
    [ Svg.polyline [ points "15 3 21 3 21 9" ] []
    , Svg.polyline [ points "9 21 3 21 3 15" ] []
    , Svg.line [ x1 "21", y1 "3", x2 "14", y2 "10" ] []
    , Svg.line [ x1 "3", y1 "21", x2 "10", y2 "14" ] []
    ]


moreVertical : List (Svg msg)
moreVertical =
    [ Svg.circle [ cx "12", cy "12", r "2" ] []
    , Svg.circle [ cx "12", cy "4", r "2" ] []
    , Svg.circle [ cx "12", cy "20", r "2" ] []
    ]


move : List (Svg msg)
move =
    [ Svg.polyline [ points "5 9 2 12 5 15" ] []
    , Svg.polyline [ points "9 5 12 2 15 5" ] []
    , Svg.polyline [ points "15 19 12 22 9 19" ] []
    , Svg.polyline [ points "19 9 22 12 19 15" ] []
    , Svg.line [ x1 "2", y1 "12", x2 "22", y2 "12" ] []
    , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "22" ] []
    ]


octagon : List (Svg msg)
octagon =
    [ Svg.polygon [ points "7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2" ] [] ]


rotateCcw : List (Svg msg)
rotateCcw =
    [ Svg.polyline [ points "1 4 1 10 7 10" ] []
    , Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] []
    ]


save : List (Svg msg)
save =
    [ Svg.path [ d "M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z" ] []
    , Svg.polyline [ points "17 21 17 13 7 13 7 21" ] []
    , Svg.polyline [ points "7 3 7 8 15 8" ] []
    ]


trash2 : List (Svg msg)
trash2 =
    [ Svg.polyline [ points "3 6 5 6 21 6" ] []
    , Svg.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
    , Svg.line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
    , Svg.line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
    ]


zoomIn : List (Svg msg)
zoomIn =
    [ Svg.circle [ cx "11", cy "11", r "8" ] []
    , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
    , Svg.line [ x1 "11", y1 "8", x2 "11", y2 "14" ] []
    , Svg.line [ x1 "8", y1 "11", x2 "14", y2 "11" ] []
    ]


zoomOut : List (Svg msg)
zoomOut =
    [ Svg.circle [ cx "11", cy "11", r "8" ] []
    , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
    , Svg.line [ x1 "8", y1 "11", x2 "14", y2 "11" ] []
    ]


zoomFit : List (Svg msg)
zoomFit =
    [ Svg.circle [ cx "11", cy "11", r "8" ] []
    , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
    , Svg.path [ d "M 6 8 v 6 h 10 v -6 h -10" ] []
    ]


boundingBox : List (Svg msg)
boundingBox =
    [ Svg.path [ d "M 23 17 h -6 m -3 0 H 4 V 7 H 20 V 11 m 0 3 v 6" ] []
    ]


contour : List (Svg msg)
contour =
    [ Svg.polygon [ points "12 22, 2 11, 7 2, 12 9, 17 2, 22 11" ] []
    , Svg.circle [ cx "12", cy "22", r "1" ] []
    , Svg.circle [ cx "2", cy "11", r "1" ] []
    , Svg.circle [ cx "7", cy "2", r "1" ] []
    , Svg.circle [ cx "12", cy "9", r "1" ] []
    , Svg.circle [ cx "17", cy "2", r "1" ] []
    , Svg.circle [ cx "22", cy "11", r "1" ] []
    ]


outline : List (Svg msg)
outline =
    [ Svg.path [ d "M12,22 Q1,15 4,6 t6,0 t6,0 t6,0 T12,22" ] [] ]
        |> Svg.g [ transform "rotate(90 12 12)" ]
        |> List.singleton
