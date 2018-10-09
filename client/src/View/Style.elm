-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Style exposing (base, focusedItemBG, hoveredItemBG, sidebarBG)

import Element
import Element.Font


base : List (Element.Attribute msg)
base =
    [ Element.Font.size 32
    ]


sidebarBG : Element.Color
sidebarBG =
    Element.rgba255 255 255 255 0.8


hoveredItemBG : Element.Color
hoveredItemBG =
    Element.rgba255 180 180 180 0.8


focusedItemBG : Element.Color
focusedItemBG =
    Element.rgba255 180 180 180 0.9
