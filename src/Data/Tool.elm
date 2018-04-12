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
