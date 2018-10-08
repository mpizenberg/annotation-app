-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ActionBar exposing (Msg, nothingProvided)

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes
import Json.Encode as Encode exposing (Value)
import Packages.FileInput as FileInput
import View.Icon as Icon



-- TYPES #############################################################


type alias Msg msg =
    { loadImages : List { name : String, file : Value } -> msg
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


loadImagesButton : (List { name : String, file : Value } -> msg) -> Element msg
loadImagesButton loadImagesMsg =
    let
        uniqueId =
            "load-images"

        imageIcon =
            [ Icon.toHtml 60 Icon.image ]
                |> Html.label
                    [ Html.Attributes.style "width" "100px"
                    , Html.Attributes.style "height" "100px"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.for uniqueId
                    ]

        invisibleInput =
            FileInput.invisible
                { id = uniqueId
                , accept = "image/*"
                , quantity = FileInput.MultipleWith loadImagesMsg
                }
    in
    Element.row [] [ Element.html imageIcon, Element.html invisibleInput ]
