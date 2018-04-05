module Dataset exposing (..)

import Element exposing (Element, below, column, el, empty, row, text)
import Element.Attributes as Attributes exposing (fill, padding, spacing, toAttr)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)
import Types exposing (..)


viewOne : (Int -> msg) -> Int -> ImageData -> Element Style var msg
viewOne msgTagger selectedId imageData =
    let
        helper isSelected id textContent =
            let
                attributes =
                    if isSelected then
                        [ padding 10 ]
                    else
                        [ padding 10, toAttr <| Pointer.onDown (always <| msgTagger id) ]
            in
            if isSelected then
                el (Style.ClassItem Style.SelectedClass) attributes textContent
            else
                el (Style.ClassItem Style.NonSelectedClass) attributes textContent
    in
    case imageData of
        EmptyImageData ->
            empty

        Loading id name ->
            helper (id == selectedId) id <|
                text ("... " ++ name)

        Loaded id name image toolsData ->
            helper (id == selectedId) id <|
                text name


viewAll : (Int -> msg) -> Zipper ImageData -> Element Style var msg
viewAll msgTagger dataset =
    case Zipper.getC dataset of
        EmptyImageData ->
            empty

        Loading selectedId imageData ->
            column Style.None [] <|
                List.concat
                    [ Zipper.getL dataset
                        |> List.map (viewOne msgTagger selectedId)
                    , [ viewOne msgTagger selectedId (Zipper.getC dataset) ]
                    , Zipper.getR dataset
                        |> List.map (viewOne msgTagger selectedId)
                    ]

        Loaded selectedId imageData _ _ ->
            column Style.None [] <|
                List.concat
                    [ Zipper.getL dataset
                        |> List.map (viewOne msgTagger selectedId)
                    , [ viewOne msgTagger selectedId (Zipper.getC dataset) ]
                    , Zipper.getR dataset
                        |> List.map (viewOne msgTagger selectedId)
                    ]
