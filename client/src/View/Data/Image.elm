-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Data.Image exposing (Image, Status(..))


type alias Image =
    { id : Int
    , name : String
    , status : Status
    }


type Status
    = Loading
    | LoadingError
    | Loaded
    | LoadedWithAnnotations
