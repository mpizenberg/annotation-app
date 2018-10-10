-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Main exposing (Msg, imagesProvided, nothingProvided)

import Data.Config as Config
import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Data.State as State
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Html
import Html.Attributes
import Json.Decode as Decode
import Packages.Zipper as Zipper
import Svg
import Svg.Attributes
import View.ActionBar as ActionBar
import View.Icon as Icon
import View.ImagesSidebar as ImagesSidebar
import View.Style as Style
import Viewer exposing (Viewer)
import Viewer.Svg



-- TYPES #############################################################


type alias Msg msg =
    { selectImage : Int -> msg
    , toggleImagesPanel : msg
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


imagesProvided : Msg msg -> State.Error -> Bool -> State.RemoteZipper -> Viewer -> Element msg
imagesProvided msg error visible remoteZipper viewer =
    let
        actionBar =
            ActionBar.imagesProvided msg.actionBar

        chevronLeft =
            Icon.toHtml 64 Icon.chevronLeft
                |> Element.html
                |> Element.el
                    [ Element.Background.color Style.sidebarBG
                    , Element.Events.onClick msg.toggleImagesPanel
                    ]

        chevronRight =
            Icon.toHtml 64 Icon.chevronRight
                |> Element.html
                |> Element.el
                    [ Element.Background.color Style.sidebarBG
                    , Element.Events.onClick msg.toggleImagesPanel
                    ]

        imagesSidebar =
            Element.row
                [ Element.alignRight
                , Element.htmlAttribute (Html.Attributes.style "height" "inherit")
                ]
                (if visible then
                    [ chevronRight
                    , imagesList
                    ]

                 else
                    [ chevronLeft ]
                )

        imagesList =
            ImagesSidebar.column
                [ Element.width (Element.maximum 600 Element.shrink)
                , Element.height Element.fill
                , Element.alignRight
                , Element.htmlAttribute (Html.Attributes.style "overflow-x" "hidden")
                , Element.scrollbarY
                , Element.Background.color Style.sidebarBG
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
