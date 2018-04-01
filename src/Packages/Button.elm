-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Packages.Button exposing (..)

import Element exposing (Element, el)
import Element.Attributes as Attributes exposing (center, px, verticalCenter)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode


type alias Button style variation msg =
    { actionability : Actionability
    , action : Element.Attribute variation msg
    , innerElement : Element style variation msg
    , innerStyle : style
    , size : ( Float, Float )
    , outerStyle : style
    , otherAttributes : List (Element.Attribute variation msg)
    }


type Actionability
    = Disabled
    | Abled State


type State
    = Inactive
    | Active


view : Button style variation msg -> Element style variation msg
view button =
    let
        ( width, height ) =
            button.size

        sizeAttributes =
            [ Attributes.width (px width), Attributes.height (px height) ]

        attributes =
            case button.actionability of
                Abled _ ->
                    button.action :: sizeAttributes ++ button.otherAttributes

                Disabled ->
                    sizeAttributes ++ button.otherAttributes

        innerButton =
            el button.innerStyle [ center, verticalCenter ] button.innerElement
    in
    el button.outerStyle attributes innerButton


type alias FileLoader style variation msg =
    { msgTagger : Decode.Value -> msg
    , uniqueId : String
    , innerElement : Element style variation msg
    , size : Float
    , noStyle : style
    , outerStyle : style
    }


loadFileInput : FileLoader style var msg -> Element style var msg
loadFileInput config =
    let
        invisibleInput =
            Html.input
                [ Html.Attributes.id config.uniqueId
                , Html.Attributes.type_ "file"
                , Html.Attributes.style [ ( "display", "none" ) ]
                , loadFileEvent config.msgTagger
                ]
                []

        labelButton =
            view
                { actionability = Abled Inactive
                , action = Html.Attributes.for config.uniqueId |> Attributes.toAttr
                , innerElement = config.innerElement
                , innerStyle = config.noStyle
                , size = ( config.size, config.size )
                , outerStyle = config.outerStyle
                , otherAttributes = []
                }
    in
    Element.row config.noStyle [] [ Element.html invisibleInput, Element.node "label" labelButton ]


loadFileEvent : (Decode.Value -> msg) -> Html.Attribute msg
loadFileEvent tagger =
    Decode.at [ "target", "files", "0" ] Decode.value
        |> Decode.map tagger
        |> Html.Events.onWithOptions "change" stopAndPrevent


stopAndPrevent : Html.Events.Options
stopAndPrevent =
    { stopPropagation = True
    , preventDefault = True
    }
