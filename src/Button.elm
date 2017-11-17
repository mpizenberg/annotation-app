-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Button exposing (..)

import Element exposing (Element, el)
import Element.Attributes as Attributes exposing (center, px, verticalCenter)


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
