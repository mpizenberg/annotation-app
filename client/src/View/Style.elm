-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Style exposing (base)

import Element
import Element.Font


base : List (Element.Attribute msg)
base =
    [ Element.Font.size 32
    ]
