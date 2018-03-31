module Annotation exposing (..)

import Array exposing (Array)


type alias Config =
    { classes : List String
    , kinds : Array Kind
    }


type alias Kind =
    { annotationType : Type
    , variants : Array String
    }


type Type
    = Point
    | Stroke
    | BBox
    | Polygone
    | Outline
