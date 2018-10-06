module Packages.Zipper exposing
    ( Zipper(..)
    , append
    , getAll
    , getC
    , getL
    , getLast
    , getR
    , goEnd
    , goL
    , goR
    , goStart
    , goTo
    , goToL
    , goToR
    , hasL
    , hasR
    , init
    , insertL
    , insertR
    , mapAll
    , moveMapStart
    , setC
    , setL
    , setR
    , updateC
    )

import Packages.ListExtra as ListExtra


type Zipper a
    = Zipper (List a) a (List a)


init : List a -> a -> List a -> Zipper a
init =
    Zipper



-- HAS #####################################################


hasL : Zipper a -> Bool
hasL (Zipper left _ _) =
    case left of
        _ :: _ ->
            True

        _ ->
            False


hasR : Zipper a -> Bool
hasR (Zipper _ _ right) =
    case right of
        _ :: _ ->
            True

        _ ->
            False



-- MOVE ####################################################


goL : Zipper a -> Zipper a
goL ((Zipper left center right) as zipper) =
    case left of
        [] ->
            zipper

        x :: xs ->
            Zipper xs x (center :: right)


goTo : (a -> Int) -> Int -> Zipper a -> Zipper a
goTo f targetId ((Zipper _ center _) as zipper) =
    if f center < targetId then
        goToR f targetId zipper

    else if f center > targetId then
        goToL f targetId zipper

    else
        zipper


goToL : (a -> Int) -> Int -> Zipper a -> Zipper a
goToL f targetId ((Zipper left center right) as zipper) =
    if f center > targetId then
        case left of
            [] ->
                zipper

            x :: xs ->
                goToL f targetId (Zipper xs x (center :: right))

    else
        zipper


goToR : (a -> Int) -> Int -> Zipper a -> Zipper a
goToR f targetId ((Zipper left center right) as zipper) =
    if f center < targetId then
        case right of
            [] ->
                zipper

            x :: xs ->
                goToR f targetId (Zipper (center :: left) x xs)

    else
        zipper


goR : Zipper a -> Zipper a
goR ((Zipper left center right) as zipper) =
    case right of
        [] ->
            zipper

        x :: xs ->
            Zipper (center :: left) x xs


goStart : Zipper a -> Zipper a
goStart ((Zipper left center right) as zipper) =
    case left of
        [] ->
            zipper

        x :: xs ->
            goStart (Zipper xs x (center :: right))


goEnd : Zipper a -> Zipper a
goEnd ((Zipper left center right) as zipper) =
    case right of
        [] ->
            zipper

        x :: xs ->
            goEnd (Zipper (center :: left) x xs)



-- GET #####################################################


getL : Zipper a -> List a
getL (Zipper left _ _) =
    List.reverse left


getC : Zipper a -> a
getC (Zipper _ center _) =
    center


getR : Zipper a -> List a
getR (Zipper _ _ right) =
    right


getAll : Zipper a -> List a
getAll (Zipper left center right) =
    case left of
        [] ->
            center :: right

        x :: xs ->
            getAll (Zipper xs x (center :: right))


getLast : Zipper a -> a
getLast (Zipper _ center right) =
    ListExtra.last right
        |> Maybe.withDefault center



-- SET #####################################################


setL : List a -> Zipper a -> Zipper a
setL left (Zipper _ center right) =
    Zipper (List.reverse left) center right


setC : a -> Zipper a -> Zipper a
setC center (Zipper left _ right) =
    Zipper left center right


setR : List a -> Zipper a -> Zipper a
setR right (Zipper left center _) =
    Zipper left center right



-- UPDATE ##################################################


updateC : (a -> a) -> Zipper a -> Zipper a
updateC f (Zipper left center right) =
    Zipper left (f center) right


mapAll : (a -> b) -> Zipper a -> Zipper b
mapAll f (Zipper left center right) =
    Zipper
        (List.map f left)
        (f center)
        (List.map f right)


moveMapStart : (a -> a) -> Zipper a -> Zipper a
moveMapStart f (Zipper left center right) =
    case left of
        [] ->
            Zipper [] (f center) right

        x :: xs ->
            moveMapStart f (Zipper xs x <| f center :: right)



-- INSERT ##################################################


insertL : a -> Zipper a -> Zipper a
insertL value (Zipper left center right) =
    Zipper (value :: left) center right


insertR : a -> Zipper a -> Zipper a
insertR value (Zipper left center right) =
    Zipper left center (value :: right)


append : List a -> Zipper a -> Zipper a
append list (Zipper left center right) =
    Zipper left center (right ++ list)
