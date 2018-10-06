module Packages.ListExtra exposing (last)


last : List a -> Maybe a
last list =
    case list of
        x :: [] ->
            Just x

        _ :: xs ->
            last xs

        [] ->
            Nothing
