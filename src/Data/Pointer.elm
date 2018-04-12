-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Pointer exposing (DragState(..), Msg(..))


type DragState
    = NoDrag
    | DraggingFrom ( Float, Float )


type Msg
    = DownAt ( Float, Float )
    | MoveAt ( Float, Float )
    | UpAt ( Float, Float )
