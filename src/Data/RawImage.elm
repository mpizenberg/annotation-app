module Data.RawImage exposing (..)

import Image exposing (Image)


-- TYPES #############################################################


type alias RawImage =
    { id : Int
    , name : String
    , status : LoadingStatus
    }


type LoadingStatus
    = Loading
    | Loaded Image
    | LoadingError String
