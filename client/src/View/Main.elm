-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Main exposing (Msg, nothingProvided)

import Data.Config as Config
import Data.State as State
import Element exposing (Element)
import Html
import Json.Decode as Decode
import View.ActionBar as ActionBar



-- TYPES #############################################################


type alias Msg msg =
    { actionBar : ActionBar.Msg msg
    }



-- type alias Parameters msg =
--     { device : Device
--     , actionBar : ActionBar.Parameters msg
--     , annotationsArea : AnnotationsArea.Parameters msg
--     , selectClassMsg : Int -> msg
--     , selectImageMsg : Int -> msg
--     }
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
    Element.column [] [ actionBar, centerArea ]
