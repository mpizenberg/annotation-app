-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ImagesSidebar exposing (verticalList)

import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Element exposing (Element)
import Element.Events
import Packages.Zipper as Zipper exposing (Zipper)


verticalList : Element.Attribute msg -> (Int -> msg) -> Zipper { id : Int, remoteImage : RemoteImage } -> Element msg
verticalList position selectImageMsg zipper =
    Element.column [ position ] <|
        List.concat
            [ Zipper.getL zipper
                |> List.map (imageItem selectImageMsg)
            , [ imageItem selectImageMsg (Zipper.getC zipper) ]
            , Zipper.getR zipper
                |> List.map (imageItem selectImageMsg)
            ]


imageItem : (Int -> msg) -> { id : Int, remoteImage : RemoteImage } -> Element msg
imageItem selectImageMsg { id, remoteImage } =
    Element.row
        [ Element.width Element.fill
        , Element.padding 10
        , Element.Events.onClick (selectImageMsg id)
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
