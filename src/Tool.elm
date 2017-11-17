-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Tool exposing (..)

import Element exposing (Element)
import Html.Lazy exposing (lazy2)
import Icons


type Tool
    = Move
    | BBox
    | Contour
    | Outline
    | Stroke
    | Point


allAnnotationTools : List Tool
allAnnotationTools =
    [ BBox, Contour, Outline, Stroke, Point ]


svgElement : Float -> Tool -> Element style variation msg
svgElement size tool =
    let
        svgIcon =
            case tool of
                Move ->
                    Icons.move

                BBox ->
                    Icons.boundingBox

                Contour ->
                    Icons.contour

                Outline ->
                    Icons.outline

                Stroke ->
                    Icons.stroke

                Point ->
                    Icons.point
    in
    lazy2 Icons.sized size svgIcon
        |> Element.html
