-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ActionBar exposing (Msg, imagesProvided, nothingProvided)

import Element exposing (Element)
import Element.Background
import Html exposing (Html)
import Html.Attributes
import Json.Encode as Encode exposing (Value)
import Packages.FileInput as FileInput
import View.Icon as Icon
import View.Style as Style



-- TYPES #############################################################


type alias Msg msg =
    { loadImages : List { name : String, file : Value } -> msg
    , loadConfig : Value -> msg
    }



-- type alias Parameters msg =
--     { size : ( Float, Float )
--     , hasAnnotations : Bool
--     , mturkMode : Bool
--
--     -- events
--     , removeLatestAnnotationMsg : msg
--     , selectToolMsg : Int -> msg
--     , zoomInMsg : msg
--     , zoomOutMsg : msg
--     , zoomFitMsg : msg
--     , loadConfigMsg : Value -> msg
--     , loadImagesMsg : List { name : String, file : Value } -> msg
--     , exportMsg : msg
--     }
-- FUNCTIONS #########################################################


nothingProvided : Msg msg -> Element msg
nothingProvided msg =
    Element.row [ Element.alignRight ]
        [ Element.text "Load images →"
        , loadImagesButton msg.loadImages
        ]


imagesProvided : Msg msg -> Element msg
imagesProvided msg =
    Element.row [ Element.alignRight ]
        [ Element.text "Load config →"
        , loadConfigButton msg.loadConfig
        , loadImagesButton msg.loadImages
        ]


loadConfigButton : (Value -> msg) -> Element msg
loadConfigButton loadConfigMsg =
    let
        uniqueId =
            "load-config"

        icon =
            [ Icon.toHtml 60 Icon.settings ]
                |> Html.label
                    [ Html.Attributes.style "width" "100px"
                    , Html.Attributes.style "height" "100px"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.for uniqueId

                    -- need to manually add a cursor because the class given by elm-ui
                    -- gets overwritten by user agent stylesheet for a label
                    , Html.Attributes.style "cursor" "pointer"
                    ]
                |> Element.html
                |> Element.el
                    [ Element.mouseOver [ Element.Background.color Style.hoveredItemBG ] ]

        invisibleInput =
            FileInput.invisible
                { id = uniqueId
                , accept = "application/json"
                , quantity = FileInput.SingleWith loadConfigMsg
                }
    in
    Element.row [] [ icon, Element.html invisibleInput ]


loadImagesButton : (List { name : String, file : Value } -> msg) -> Element msg
loadImagesButton loadImagesMsg =
    let
        uniqueId =
            "load-images"

        icon =
            [ Icon.toHtml 60 Icon.image ]
                |> Html.label
                    [ Html.Attributes.style "width" "100px"
                    , Html.Attributes.style "height" "100px"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.for uniqueId

                    -- need to manually add a cursor because the class given by elm-ui
                    -- gets overwritten by user agent stylesheet for a label
                    , Html.Attributes.style "cursor" "pointer"
                    ]
                |> Element.html
                |> Element.el
                    [ Element.mouseOver [ Element.Background.color Style.hoveredItemBG ] ]

        invisibleInput =
            FileInput.invisible
                { id = uniqueId
                , accept = "image/*"
                , quantity = FileInput.MultipleWith loadImagesMsg
                }
    in
    Element.row [] [ icon, Element.html invisibleInput ]
