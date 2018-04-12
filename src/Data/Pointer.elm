module Data.Pointer exposing (DragState(..), Msg(..))


type DragState
    = NoDrag
    | DraggingFrom ( Float, Float )


type Msg
    = DownAt ( Float, Float )
    | MoveAt ( Float, Float )
    | UpAt ( Float, Float )
