-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Tool exposing (..)

import Element exposing (Element)
import Icons


type Tool
    = Move
    | Contour
    | BBox
    | Outline


allAnnotationTools : List Tool
allAnnotationTools =
    [ Contour, BBox, Outline ]


svgElement : Float -> Tool -> Element style variation msg
svgElement size tool =
    let
        svgIcon =
            case tool of
                Move ->
                    Icons.move

                Contour ->
                    Icons.octagon

                BBox ->
                    Icons.maximize2

                Outline ->
                    Icons.download
    in
    Icons.sized size svgIcon
        |> Element.html
