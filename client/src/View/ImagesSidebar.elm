-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ImagesSidebar exposing (column)

import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Element exposing (Element)
import Element.Background
import Element.Events
import Packages.Zipper as Zipper exposing (Zipper)
import View.Style as Style


column : List (Element.Attribute msg) -> (Int -> msg) -> Zipper { id : Int, remoteImage : RemoteImage } -> Element msg
column attributes selectImageMsg zipper =
    Element.column attributes <|
        List.concat
            [ Zipper.getL zipper
                |> List.map (imageItem selectImageMsg Style.sidebarBG)
            , [ imageItem selectImageMsg Style.focusedItemBG (Zipper.getC zipper) ]
            , Zipper.getR zipper
                |> List.map (imageItem selectImageMsg Style.sidebarBG)
            ]


imageItem : (Int -> msg) -> Element.Color -> { id : Int, remoteImage : RemoteImage } -> Element msg
imageItem selectImageMsg bgColor { id, remoteImage } =
    Element.row
        [ Element.width Element.fill
        , Element.padding 10
        , Element.pointer
        , Element.Events.onClick (selectImageMsg id)
        , Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
        , Element.Background.color bgColor
        ]
        [ indicator remoteImage.status
        , Element.text remoteImage.name
        ]


indicator : RemoteImage.LoadingStatus -> Element msg
indicator status =
    case status of
        RemoteImage.Loading ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "...")

        RemoteImage.Loaded _ ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "")

        RemoteImage.LoadingError _ ->
            Element.el [ Element.width (Element.px 30) ] (Element.text "X")
