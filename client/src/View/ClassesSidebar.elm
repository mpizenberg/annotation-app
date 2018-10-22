-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ClassesSidebar exposing (column)

import Data.State as State
import Element exposing (Element)
import Element.Background
import Element.Events
import Element.Font
import Packages.FileSystem as FileSystem exposing (File, FileSystem, Folder)
import View.Style as Style


column : List (Element.Attribute msg) -> (Int -> msg) -> (( Int, Int ) -> msg) -> State.Classes -> Element msg
column attributes toggleCategoryMsg selectClassMsg classes =
    foldableContents attributes toggleCategoryMsg selectClassMsg (FileSystem.root classes.all)



-- From FileSystem view code


foldable : (Int -> msg) -> (( Int, Int ) -> msg) -> FileSystem -> Element msg
foldable toggleCategoryMsg selectClassMsg fileSystem =
    Element.column [ Element.width Element.fill ]
        [ folder toggleCategoryMsg (FileSystem.currentFolder fileSystem)
        , Element.row
            [ Element.width Element.fill ]
            [ space 50
            , foldableContents [ Element.width Element.fill ] toggleCategoryMsg selectClassMsg fileSystem
            ]
        ]


space : Int -> Element msg
space width =
    Element.el [ Element.width <| Element.px width ] Element.none


foldableContents : List (Element.Attribute msg) -> (Int -> msg) -> (( Int, Int ) -> msg) -> FileSystem -> Element msg
foldableContents attributes toggleCategoryMsg selectClassMsg fileSystem =
    let
        subfoldersElements =
            subfolders toggleCategoryMsg selectClassMsg fileSystem

        filesElements =
            subfiles selectClassMsg (FileSystem.currentFolder fileSystem)
    in
    Element.column attributes (subfoldersElements ++ filesElements)



-- Sub folders


subfolders : (Int -> msg) -> (( Int, Int ) -> msg) -> FileSystem -> List (Element msg)
subfolders toggleCategoryMsg selectClassMsg fileSystem =
    if .open (FileSystem.currentFolder fileSystem) then
        FileSystem.subfolders fileSystem
            |> List.map (foldable toggleCategoryMsg selectClassMsg)

    else
        []



-- Just folder


folder : (Int -> msg) -> Folder -> Element msg
folder toggleCategoryMsg { id, open, name } =
    Element.row
        [ Element.Events.onClick (toggleCategoryMsg id)
        , Element.width Element.fill
        , Element.padding 10
        , Element.pointer
        , Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
        , Element.Background.color Style.sidebarBG
        ]
        [ openOrClosedIcon 50 open
        , Element.el [ Element.width Element.fill ] (Element.text name)
        ]


openOrClosedIcon : Int -> Bool -> Element msg
openOrClosedIcon width open =
    if open then
        Element.el [ Element.width <| Element.px width, Element.Font.center ] (Element.text "–")

    else
        Element.el [ Element.width <| Element.px width, Element.Font.center ] (Element.text "+")



-- Files in folder


subfiles : (( Int, Int ) -> msg) -> Folder -> List (Element msg)
subfiles selectClassMsg { id, open, files } =
    if open then
        List.map (file selectClassMsg id) files

    else
        []


file : (( Int, Int ) -> msg) -> Int -> File -> Element msg
file selectClassMsg folderId { id, name } =
    Element.row
        [ Element.Events.onClick (selectClassMsg ( folderId, id ))
        , Element.width Element.fill
        , Element.padding 10
        , Element.pointer
        , Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
        , Element.Background.color Style.sidebarBG
        ]
        [ fileIcon 50
        , Element.text name
        ]


fileIcon : Int -> Element msg
fileIcon width =
    Element.el [ Element.width <| Element.px width, Element.Font.center ] (Element.text ":")
