-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.DatasetSideBar exposing (viewAnnotated, viewRaw)

import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.RawImage as RawImage exposing (RawImage)
import Element exposing (Element, column, el, empty, text)
import Element.Attributes as Attributes exposing (padding, paddingLeft)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)


viewRaw : (Int -> msg) -> Zipper RawImage -> Element Style var msg
viewRaw selectImageMsg dataset =
    column Style.None [] <|
        List.concat
            [ Zipper.getL dataset
                |> List.map (viewOne selectImageMsg False identity)
            , [ viewOne selectImageMsg True identity (Zipper.getC dataset) ]
            , Zipper.getR dataset
                |> List.map (viewOne selectImageMsg False identity)
            ]


viewAnnotated : (Int -> msg) -> Zipper AnnotatedImage -> Element Style var msg
viewAnnotated selectImageMsg dataset =
    if List.isEmpty (Zipper.getL dataset) && List.isEmpty (Zipper.getR dataset) then
        empty

    else
        column Style.None [] <|
            List.concat
                [ Zipper.getL dataset
                    |> List.map (viewOne selectImageMsg False toRawLoading)
                , [ viewOne selectImageMsg True toRawLoading (Zipper.getC dataset) ]
                , Zipper.getR dataset
                    |> List.map (viewOne selectImageMsg False toRawLoading)
                ]


toRawLoading : AnnotatedImage.Status -> RawImage.LoadingStatus
toRawLoading status =
    case status of
        AnnotatedImage.Loading ->
            RawImage.Loading

        AnnotatedImage.Loaded img _ ->
            RawImage.Loaded img

        AnnotatedImage.LoadingError err ->
            RawImage.LoadingError err


viewOne : (Int -> msg) -> Bool -> (status -> RawImage.LoadingStatus) -> { id : Int, name : String, status : status } -> Element Style var msg
viewOne selectImageMsg isSelected toLoadingStatus { id, name, status } =
    let
        helper isSelected id textContent =
            let
                attributes =
                    if isSelected then
                        [ padding 10 ]

                    else
                        [ padding 10
                        , Pointer.onDown (always <| selectImageMsg id)
                            |> Attributes.toAttr
                        ]
            in
            if isSelected then
                el (Style.ClassItem Style.SelectedClass) attributes textContent

            else
                el (Style.ClassItem Style.NonSelectedClass) attributes textContent
    in
    case toLoadingStatus status of
        RawImage.Loading ->
            helper isSelected id <|
                text ("... " ++ name)

        RawImage.Loaded _ ->
            helper isSelected id <|
                text name

        RawImage.LoadingError _ ->
            helper isSelected id <|
                text ("X  " ++ name)
