module View.Classes exposing (..)

import Element exposing (Element, below, column, el, empty)
import Element.Attributes as Attributes exposing (alignLeft, fill)
import StyleSheet as Style exposing (Style)


view : List String -> Element Style var msg
view classes =
    classes
        |> List.map Element.text
        |> column Style.None []
