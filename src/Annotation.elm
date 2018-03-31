module Annotation exposing (..)

import Annotation.Geometry.Types exposing (..)


emptyConfig : Config
emptyConfig =
    { classes = []
    , kinds =
        [ Kind PointType [ "", "" ]
        , Kind BBoxType []
        ]
    }


type alias Config =
    { classes : List String
    , kinds : List Kind
    }


type alias Kind =
    { annotationType : Type
    , variants : List String
    }


type Type
    = PointType
    | BBoxType


type Annotations
    = Point PointDrawings
    | BBox BBoxDrawings


type alias PointDrawings =
    {}


type alias BBoxDrawings =
    {}


type OutlineDrawing
    = NoOutline
    | DrawingOutline Stroke
    | EndedOutline Outline


type ContourDrawing
    = NoContour
    | DrawingStartedAt ( Float, Float ) Stroke
    | Ended Contour
