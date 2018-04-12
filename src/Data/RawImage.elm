module Data.RawImage exposing (LoadingStatus(..), RawImage)

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
