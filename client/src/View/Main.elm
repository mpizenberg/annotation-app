-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Main exposing (Msg, imagesProvided, nothingProvided)

import Data.Config as Config
import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Data.State as State
import Element exposing (Element)
import Html
import Html.Attributes
import Json.Decode as Decode
import Packages.Zipper as Zipper
import Svg
import Svg.Attributes
import View.ActionBar as ActionBar
import View.ImagesSidebar as ImagesSidebar
import Viewer exposing (Viewer)
import Viewer.Svg



-- TYPES #############################################################


type alias Msg msg =
    { selectImage : Int -> msg
    , actionBar : ActionBar.Msg msg
    }



-- FUNCTIONS #########################################################


nothingProvided : Msg msg -> State.Error -> Element msg
nothingProvided msg error =
    let
        actionBar =
            ActionBar.nothingProvided msg.actionBar

        centerArea =
            case error of
                State.NoError ->
                    Element.none

                State.ConfigError Config.IncorrectClasses ->
                    Element.text "Classes in config are incorrect"

                State.ConfigError Config.IncorrectTools ->
                    Element.text "Tools in config are incorrect"

                State.ConfigError (Config.Incorrect decodeError) ->
                    Html.pre [] [ Html.text (Decode.errorToString decodeError) ]
                        |> Element.html
    in
    Element.column [ Element.width Element.fill ] [ actionBar, centerArea ]


imagesProvided : Msg msg -> State.Error -> State.RemoteZipper -> Viewer -> Element msg
imagesProvided msg error remoteZipper viewer =
    let
        actionBar =
            ActionBar.nothingProvided msg.actionBar

        imagesSidebar =
            ImagesSidebar.verticalList
                [ Element.alignRight
                , Element.clip
                , Element.width (Element.maximum 200 Element.shrink)
                , Element.htmlAttribute (Html.Attributes.style "height" "inherit")
                , Element.scrollbarY
                , Element.htmlAttribute (Html.Attributes.style "overflow-x" "hidden")
                ]
                msg.selectImage
                remoteZipper

        centerArea =
            case error of
                State.NoError ->
                    imageArea (.remoteImage (Zipper.getC remoteZipper)) viewer

                State.ConfigError Config.IncorrectClasses ->
                    Element.text "Classes in config are incorrect"

                State.ConfigError Config.IncorrectTools ->
                    Element.text "Tools in config are incorrect"

                State.ConfigError (Config.Incorrect decodeError) ->
                    Html.pre [] [ Html.text (Decode.errorToString decodeError) ]
                        |> Element.html

        centerAreaWithSidebars =
            centerArea
                |> Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.inFront imagesSidebar
                    ]
    in
    Element.column
        [ Element.width Element.fill, Element.height Element.fill ]
        [ actionBar, centerAreaWithSidebars ]


imageArea : RemoteImage -> Viewer -> Element msg
imageArea remoteImage viewer =
    case remoteImage.status of
        RemoteImage.Loading ->
            Element.text "Image loading ..."

        RemoteImage.LoadingError errorMsg ->
            Element.text errorMsg

        RemoteImage.Loaded image ->
            let
                svgImage =
                    Svg.image
                        [ Svg.Attributes.xlinkHref image.url
                        , Svg.Attributes.width (String.fromInt image.width)
                        , Svg.Attributes.height (String.fromInt image.height)
                        ]
                        []
            in
            [ Viewer.Svg.placeIn viewer [ svgImage ] ]
                |> Svg.svg
                    [ Html.Attributes.style "flex" "1"
                    , Html.Attributes.style "width" "100%"
                    ]
                |> Element.html
