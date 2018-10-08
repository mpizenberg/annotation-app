-- Module containing svg icons generated from 1602/elm-feather-icons
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Icon exposing
    ( image
    , toHtml
    )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


toHtml : Float -> List (Svg msg) -> Html msg
toHtml size icon =
    svg (width (String.fromFloat size) :: height (String.fromFloat size) :: defaultAttributes) icon


defaultAttributes : List (Svg.Attribute msg)
defaultAttributes =
    [ fill "none"
    , Svg.Attributes.stroke "currentColor"
    , strokeLinecap "round"
    , strokeLinejoin "round"
    , strokeWidth "2"
    , viewBox "0 0 24 24"
    ]



-- Designed by Matthieu ####################################
-- Feather icons ###########################################


image : List (Svg msg)
image =
    [ Svg.rect [ x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
    , Svg.circle [ cx "8.5", cy "8.5", r "1.5" ] []
    , Svg.polyline [ points "21 15 16 10 5 21" ] []
    ]
