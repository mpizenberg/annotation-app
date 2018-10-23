-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ImagesSidebar exposing (column)

import Element exposing (Element)
import Element.Background
import Element.Events
import Packages.Zipper as Zipper exposing (Zipper)
import View.Data.Image as Image exposing (Image, Status(..))
import View.Style as Style


column : List (Element.Attribute msg) -> (Int -> msg) -> Zipper Image -> Element msg
column attributes selectImageMsg zipper =
    Element.column attributes <|
        List.concat
            [ Zipper.getL zipper
                |> List.map (imageItem selectImageMsg Style.sidebarBG)
            , [ imageItem selectImageMsg Style.focusedItemBG (Zipper.getC zipper) ]
            , Zipper.getR zipper
                |> List.map (imageItem selectImageMsg Style.sidebarBG)
            ]


imageItem : (Int -> msg) -> Element.Color -> Image -> Element msg
imageItem selectImageMsg bgColor { id, name, status } =
    Element.row
        [ Element.width Element.fill
        , Element.padding 10
        , Element.pointer
        , Element.Events.onClick (selectImageMsg id)
        , Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
        , Element.Background.color bgColor
        ]
        [ indicator status
        , Element.text name
        ]


indicator : Status -> Element msg
indicator status =
    case status of
        Loading ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "...")

        LoadingError ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "X")

        Loaded ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "")

        LoadedWithAnnotations ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "")
