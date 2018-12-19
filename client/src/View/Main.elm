-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Main exposing (Msg, allProvided, configProvided, imagesProvided, nothingProvided)

import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.Config as Config exposing (Config)
import Data.Pointer as Pointer
import Data.RemoteImage as RemoteImage exposing (RemoteImage)
import Data.State as State
import Data.Tool as Tool exposing (Tool)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Value
import Packages.Zipper as Zipper exposing (Zipper)
import Svg
import Svg.Attributes
import View.ActionBar as ActionBar
import View.Annotation as Annotation
import View.ClassesSidebar as ClassesSidebar
import View.Data.Image as Image exposing (Image, Status(..))
import View.Icon as Icon
import View.ImagesSidebar as ImagesSidebar
import View.Style as Style
import Viewer exposing (Viewer)
import Viewer.Svg



-- TYPES #############################################################


type alias Msg msg =
    { selectImage : Int -> msg
    , classesSidebar : ClassesSidebar.Msg msg
    , toggleImagesPanel : msg
    , toggleClassesPanel : msg
    , actionBar : ActionBar.Msg msg
    , pointer : Pointer.Msg -> msg
    , rawPointer : Value -> msg
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

                State.ConfigError configError ->
                    configErrorPreformatted configError
    in
    Element.column [ Element.width Element.fill ] [ actionBar, centerArea ]


configProvided : Msg msg -> State.Error -> Config -> Bool -> State.Classes -> Zipper Tool -> Element msg
configProvided msg error config visible classes toolsZipper =
    let
        actionBar =
            ActionBar.configProvided msg.actionBar toolsZipper

        chevronLeft =
            Icon.toHtml 64 Icon.chevronLeft
                |> Element.html
                |> Element.el
                    [ Element.Background.color Style.sidebarBG
                    , Element.Events.onClick msg.toggleClassesPanel
                    ]

        chevronRight =
            Icon.toHtml 64 Icon.chevronRight
                |> Element.html
                |> Element.el
                    [ Element.Background.color Style.sidebarBG
                    , Element.Events.onClick msg.toggleClassesPanel
                    ]

        classesSidebar =
            Element.row
                [ Element.alignLeft
                , Element.htmlAttribute (Html.Attributes.style "height" "inherit")
                ]
                (if visible then
                    [ classesList
                    , chevronLeft
                    ]

                 else
                    [ chevronRight ]
                )

        classesList =
            ClassesSidebar.column
                [ Element.width (Element.maximum 600 Element.fill)
                , Element.height Element.fill
                , Element.alignLeft
                , Element.htmlAttribute (Html.Attributes.style "overflow-x" "hidden")
                , Element.scrollbarY
                , Element.Background.color Style.sidebarBG
                ]
                msg.classesSidebar
                classes

        centerArea =
            case error of
                State.NoError ->
                    Element.text "No image loaded yet"
                        |> Element.el [ Element.centerX, Element.centerY ]

                State.ConfigError configError ->
                    configErrorPreformatted configError

        centerAreaWithSidebars =
            centerArea
                |> Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.inFront classesSidebar
                    ]
    in
    Element.column
        [ Element.width Element.fill, Element.height Element.fill ]
        [ actionBar, centerAreaWithSidebars ]


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
                (Zipper.mapAll remoteToImageData remoteZipper)

        centerArea =
            case error of
                State.NoError ->
                    imageArea (.remoteImage (Zipper.getC remoteZipper)) viewer

                State.ConfigError configError ->
                    configErrorPreformatted configError

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


remoteToImageData : { id : Int, remoteImage : RemoteImage } -> Image
remoteToImageData { id, remoteImage } =
    { id = id
    , name = remoteImage.name
    , status =
        case remoteImage.status of
            RemoteImage.Loading ->
                Loading

            RemoteImage.LoadingError _ ->
                LoadingError

            RemoteImage.Loaded _ ->
                Loaded
    }


annotatedToImageData : { id : Int, annotatedImage : AnnotatedImage } -> Image
annotatedToImageData { id, annotatedImage } =
    { id = id
    , name = annotatedImage.name
    , status =
        case annotatedImage.status of
            AnnotatedImage.Loading ->
                Loading

            AnnotatedImage.LoadingError _ ->
                LoadingError

            AnnotatedImage.Loaded _ ->
                Loaded

            AnnotatedImage.LoadedWithAnnotations _ _ _ ->
                LoadedWithAnnotations
    }


configErrorPreformatted : Config.Error -> Element msg
configErrorPreformatted configError =
    case configError of
        Config.IncorrectClasses ->
            Element.text "Classes in config are incorrect"

        Config.IncorrectTools ->
            [ Element.text "You should always have at least one tool. Try adding one in the 'tools' field." ]
                |> Element.paragraph []

        Config.Incorrect decodeError ->
            Html.pre [] [ Html.text (Decode.errorToString <| foldValueInError decodeError) ]
                |> Element.html
                |> Element.el [ Element.Font.size 12 ]


foldValueInError : Decode.Error -> Decode.Error
foldValueInError error =
    case error of
        Decode.Field string err ->
            Decode.Field string (foldValueInError err)

        Decode.Index int err ->
            Decode.Index int (foldValueInError err)

        Decode.OneOf list ->
            Decode.OneOf (List.map foldValueInError list)

        Decode.Failure string value ->
            Decode.Failure string (foldValue value)


foldValue : Value -> Value
foldValue value =
    case Json.Value.decodeValue value of
        Json.Value.ObjectValue list ->
            List.map (Tuple.mapSecond strictFoldJsonValue) list
                |> Json.Value.ObjectValue
                |> Json.Value.encode

        Json.Value.ArrayValue list ->
            Json.Value.ArrayValue (List.map strictFoldJsonValue list)
                |> Json.Value.encode

        _ ->
            value


strictFoldJsonValue : Json.Value.JsonValue -> Json.Value.JsonValue
strictFoldJsonValue jsonValue =
    case jsonValue of
        Json.Value.ObjectValue _ ->
            Json.Value.StringValue "{...}"

        Json.Value.ArrayValue _ ->
            Json.Value.StringValue "[...]"

        _ ->
            jsonValue


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


annotatedImageArea : Msg msg -> AnnotatedImage -> Viewer -> Element msg
annotatedImageArea msg annotatedImage viewer =
    case annotatedImage.status of
        AnnotatedImage.Loading ->
            Element.text "Image loading ..."

        AnnotatedImage.LoadingError errorMsg ->
            Element.text errorMsg

        AnnotatedImage.Loaded image ->
            let
                svgImage =
                    Svg.image
                        [ Svg.Attributes.xlinkHref image.url
                        , Svg.Attributes.width (String.fromInt image.width)
                        , Svg.Attributes.height (String.fromInt image.height)
                        ]
                        []
            in
            [ Viewer.Svg.placeInWithDetails [ Html.Attributes.style "pointer-events" "none" ] viewer [ svgImage ] ]
                |> Svg.svg
                    [ Html.Attributes.style "flex" "1"
                    , Html.Attributes.style "width" "100%"
                    , msgOn "pointerdown" (Decode.map msg.rawPointer Decode.value)
                    , Html.Events.Extra.Pointer.onUp (.pointer >> .offsetPos >> Pointer.UpAt >> msg.pointer)
                    , Html.Events.Extra.Pointer.onMove (.pointer >> .offsetPos >> Pointer.MoveAt >> msg.pointer)

                    -- no touch-action (prevent scroll etc.)
                    , Html.Attributes.style "touch-action" "none"
                    ]
                |> Element.html

        AnnotatedImage.LoadedWithAnnotations image _ annotatedZipper ->
            let
                svgImage =
                    Svg.image
                        [ Svg.Attributes.xlinkHref image.url
                        , Svg.Attributes.width (String.fromInt image.width)
                        , Svg.Attributes.height (String.fromInt image.height)
                        ]
                        []

                svgAnnotations =
                    Zipper.getAll annotatedZipper
                        |> List.map (\{ annotation } -> Annotation.view annotation)
            in
            [ Viewer.Svg.placeInWithDetails [ Html.Attributes.style "pointer-events" "none" ] viewer [ svgImage ] ]
                |> Svg.svg
                    [ Html.Attributes.style "flex" "1"
                    , Html.Attributes.style "width" "100%"
                    , msgOn "pointerdown" (Decode.map msg.rawPointer Decode.value)
                    , Html.Events.Extra.Pointer.onUp (.pointer >> .offsetPos >> Pointer.UpAt >> msg.pointer)
                    , Html.Events.Extra.Pointer.onMove (.pointer >> .offsetPos >> Pointer.MoveAt >> msg.pointer)
                    ]
                |> Element.html


msgOn : String -> Decoder msg -> Html.Attribute msg
msgOn event =
    Decode.map (\msg -> { message = msg, stopPropagation = False, preventDefault = True })
        >> Html.Events.custom event


allProvided : Msg msg -> State.Error -> Config -> State.SidePanels -> State.Classes -> Zipper Tool -> State.AnnotatedZipper -> Viewer -> Element msg
allProvided msg error config sidePanels classes toolsZipper annotatedImages viewer =
    let
        actionBar =
            ActionBar.allProvided msg.actionBar toolsZipper

        chevronLeft action =
            Icon.toHtml 64 Icon.chevronLeft
                |> Element.html
                |> Element.el
                    [ Element.Background.color Style.sidebarBG
                    , Element.Events.onClick action
                    ]

        chevronRight action =
            Icon.toHtml 64 Icon.chevronRight
                |> Element.html
                |> Element.el
                    [ Element.Background.color Style.sidebarBG
                    , Element.Events.onClick action
                    ]

        classesSidebar =
            Element.row
                [ Element.alignLeft
                , Element.htmlAttribute (Html.Attributes.style "height" "inherit")
                ]
                (if sidePanels.classesVisible then
                    [ classesList
                    , chevronLeft msg.toggleClassesPanel
                    ]

                 else
                    [ chevronRight msg.toggleClassesPanel ]
                )

        classesList =
            ClassesSidebar.column
                [ Element.width (Element.maximum 600 Element.fill)
                , Element.height Element.fill
                , Element.alignLeft
                , Element.htmlAttribute (Html.Attributes.style "overflow-x" "hidden")
                , Element.scrollbarY
                , Element.Background.color Style.sidebarBG
                ]
                msg.classesSidebar
                classes

        imagesSidebar =
            Element.row
                [ Element.alignRight
                , Element.htmlAttribute (Html.Attributes.style "height" "inherit")
                ]
                (if sidePanels.imagesVisible then
                    [ chevronRight msg.toggleImagesPanel
                    , imagesList
                    ]

                 else
                    [ chevronLeft msg.toggleImagesPanel ]
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
                (Zipper.mapAll annotatedToImageData annotatedImages)

        centerArea =
            case error of
                State.NoError ->
                    annotatedImageArea msg (.annotatedImage (Zipper.getC annotatedImages)) viewer

                State.ConfigError configError ->
                    configErrorPreformatted configError

        centerAreaWithSidebars =
            centerArea
                |> Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.inFront classesSidebar
                    , Element.inFront imagesSidebar
                    ]
    in
    Element.column
        [ Element.width Element.fill, Element.height Element.fill ]
        [ actionBar, centerAreaWithSidebars ]
