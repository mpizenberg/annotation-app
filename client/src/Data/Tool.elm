-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Tool exposing (Tool, Type(..))

-- TYPES #############################################################


type alias Tool =
    { id : Int
    , type_ : Type
    , variant : Int
    }


type Type
    = Move
    | Point
    | BBox
    | Stroke
    | Outline
    | Polygon



-- UPDATE ############################################################
