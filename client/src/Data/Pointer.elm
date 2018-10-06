-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Data.Pointer exposing (DragState(..), Msg(..), mapMsg, update)


type DragState
    = NoDrag
    | DraggingFrom ( Float, Float )


type Msg
    = DownAt ( Float, Float )
    | MoveAt ( Float, Float )
    | UpAt ( Float, Float )


update : Msg -> DragState -> DragState
update msg dragState =
    Debug.todo "Pointer.update"


mapMsg : (( Float, Float ) -> ( Float, Float )) -> Msg -> Msg
mapMsg transform msg =
    case msg of
        DownAt pos ->
            DownAt (transform pos)

        MoveAt pos ->
            MoveAt (transform pos)

        UpAt pos ->
            UpAt (transform pos)
