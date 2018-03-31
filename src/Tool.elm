-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Tool exposing (..)

import Annotation exposing (Annotations)
import Element exposing (Element)
import Element.Attributes exposing (vary)
import Html.Lazy exposing (lazy2)
import Icons
import Json.Decode as Decode
import Packages.Zipper as Zipper exposing (Zipper)
import StyleSheet as Style exposing (Style)


type alias Data =
    { id : Int
    , tool : Tool
    , colorId : Int
    }


type Tool
    = Move
    | Annotation Annotations


fromAnnotationType : Annotation.Type -> Tool
fromAnnotationType annotationType =
    case annotationType of
        Annotation.PointType ->
            Annotation (Annotation.Point [])

        Annotation.BBoxType ->
            Annotation (Annotation.BBox [])

        Annotation.StrokeType ->
            Annotation (Annotation.Stroke [])

        Annotation.OutlineType ->
            Annotation (Annotation.Outline [])

        Annotation.PolygonType ->
            Annotation (Annotation.Polygon [])


fromConfig : Annotation.Config -> Zipper Data
fromConfig config =
    let
        moveData : Data
        moveData =
            { id = 0
            , tool = Move
            , colorId = 0
            }

        zipperWithOnlyMove : Zipper Data
        zipperWithOnlyMove =
            Zipper.init [] moveData []

        addKind : Annotation.Kind -> Zipper Data -> Zipper Data
        addKind kind zipper =
            case kind.variants of
                [] ->
                    addVariants 0 kind.annotationType [] zipper

                _ :: vs ->
                    addVariants 1 kind.annotationType vs zipper

        addVariants : Int -> Annotation.Type -> List a -> Zipper Data -> Zipper Data
        addVariants id annotationType otherVariants zipper =
            let
                currentId =
                    .id (Zipper.getC zipper)

                variantData =
                    { id = 1 + .id (Zipper.getC zipper)
                    , tool = fromAnnotationType annotationType
                    , colorId = id
                    }

                newZipper =
                    zipper
                        |> Zipper.insertR variantData
                        |> Zipper.goR
            in
            case otherVariants of
                [] ->
                    newZipper

                v :: vs ->
                    addVariants (id + 1) annotationType vs newZipper
    in
    List.foldl addKind zipperWithOnlyMove config.kinds
        |> Zipper.goStart


fromConfigString : String -> Zipper Data
fromConfigString =
    Decode.decodeString Annotation.configDecoder
        >> Result.withDefault Annotation.emptyConfig
        >> fromConfig


svgElement : Float -> Data -> Element Style Style.ColorVariations msg
svgElement size toolData =
    let
        svgIcon =
            case toolData.tool of
                Move ->
                    Icons.move

                Annotation (Annotation.Point _) ->
                    Icons.point

                Annotation (Annotation.BBox _) ->
                    Icons.boundingBox

                Annotation (Annotation.Stroke _) ->
                    Icons.stroke

                Annotation (Annotation.Outline _) ->
                    Icons.outline

                Annotation (Annotation.Polygon _) ->
                    Icons.polygon
    in
    lazy2 Icons.sized size svgIcon
        |> Element.html
        |> Element.el Style.ToolIcon [ vary (Style.FromPalette toolData.colorId) True ]
