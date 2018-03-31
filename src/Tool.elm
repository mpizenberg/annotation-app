-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Tool exposing (..)

import Annotation
import Array exposing (Array)
import Element exposing (Element)
import Element.Attributes as Attributes
import Html exposing (Html)
import Html.Lazy exposing (lazy2)
import Icons
import StyleSheet as Style exposing (Style)


type Tool
    = Move
    | BBox
    | Contour
    | Outline
    | Stroke
    | Point


type ToolBis
    = MoveBis
    | DrawTool Annotation.Type (Maybe Int)


toolsFromConfig : Annotation.Config -> Array ToolBis
toolsFromConfig config =
    let
        f kind toolArray =
            case Array.length kind.variants of
                0 ->
                    Array.push (DrawTool kind.annotationType Nothing) toolArray

                nbVariants ->
                    -- add all variants drawtool
                    List.range 0 (nbVariants - 1)
                        |> List.map (\n -> DrawTool kind.annotationType (Just n))
                        |> Array.fromList
                        |> Array.append toolArray
    in
    Array.foldl f Array.empty config.kinds


allAnnotationTools : List Tool
allAnnotationTools =
    [ BBox, Contour, Outline, Stroke, Point ]


toolSvg : Float -> ToolBis -> Element Style Style.ColorVariations msg
toolSvg size tool =
    let
        ( svgIcon, maybeVariation ) =
            case tool of
                MoveBis ->
                    ( Icons.move, Nothing )

                DrawTool Annotation.BBox var ->
                    ( Icons.boundingBox, var )

                DrawTool Annotation.Polygone var ->
                    ( Icons.contour, var )

                DrawTool Annotation.Outline var ->
                    ( Icons.outline, var )

                DrawTool Annotation.Stroke var ->
                    ( Icons.stroke, var )

                DrawTool Annotation.Point var ->
                    ( Icons.point, var )

        lazyIconElement =
            lazy2 Icons.sized size svgIcon
                |> Element.html
    in
    case maybeVariation of
        Nothing ->
            lazyIconElement

        Just var ->
            Element.el Style.ToolIcon [ Attributes.vary (Style.FromPalette var) True ] lazyIconElement


svgElement : Float -> Tool -> Html msg
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
