-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ClassesSidebar exposing (column)

import Data.State as State
import Element exposing (Element)
import Element.Background
import Element.Events
import View.Style as Style


column : List (Element.Attribute msg) -> (Int -> msg) -> (( Int, Int ) -> msg) -> State.Classes -> Element msg
column attributes toggleCategoryMsg selectClassMsg classes =
    Element.column []
        [ Element.text "14"
        , Element.text "Test"
        ]
