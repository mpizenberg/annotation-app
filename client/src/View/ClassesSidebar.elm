-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.ClassesSidebar exposing (Msg, column)

import Data.State as State
import Element exposing (Element)
import Element.Background
import Element.Events
import Element.Font
import Packages.FileSystem as FileSystem exposing (File, FileSystem, Folder)
import View.Style as Style


type alias Msg msg =
    { toggleCategory : Int -> msg
    , selectClass : ( Int, Int ) -> msg
    }


column : List (Element.Attribute msg) -> Msg msg -> State.Classes -> Element msg
column attributes msg classes =
    foldableContents attributes msg classes.selected (FileSystem.root classes.all)



-- From FileSystem view code


foldable : Msg msg -> Maybe ( Int, Int ) -> FileSystem -> Element msg
foldable msg selected fileSystem =
    Element.column [ Element.width Element.fill ]
        [ folder msg.toggleCategory (FileSystem.currentFolder fileSystem)
        , Element.row
            [ Element.width Element.fill ]
            [ space 50
            , foldableContents [ Element.width Element.fill ] msg selected fileSystem
            ]
        ]


space : Int -> Element msg
space width =
    Element.el [ Element.width <| Element.px width ] Element.none


foldableContents : List (Element.Attribute msg) -> Msg msg -> Maybe ( Int, Int ) -> FileSystem -> Element msg
foldableContents attributes msg selected fileSystem =
    let
        subfoldersElements =
            subfolders msg selected fileSystem

        filesElements =
            subfiles msg.selectClass selected (FileSystem.currentFolder fileSystem)
    in
    Element.column attributes (subfoldersElements ++ filesElements)



-- Sub folders


subfolders : Msg msg -> Maybe ( Int, Int ) -> FileSystem -> List (Element msg)
subfolders msg selected fileSystem =
    if .open (FileSystem.currentFolder fileSystem) then
        FileSystem.subfolders fileSystem
            |> List.map (foldable msg selected)

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


subfiles : (( Int, Int ) -> msg) -> Maybe ( Int, Int ) -> Folder -> List (Element msg)
subfiles selectClassMsg selected { id, open, files } =
    if open then
        List.map (file selectClassMsg selected id) files

    else
        []


file : (( Int, Int ) -> msg) -> Maybe ( Int, Int ) -> Int -> File -> Element msg
file selectClassMsg selected folderId { id, name } =
    let
        bgColor =
            case selected of
                Nothing ->
                    Style.sidebarBG

                Just ( selectedFolder, selectedClass ) ->
                    if selectedFolder == folderId && selectedClass == id then
                        Style.focusedItemBG

                    else
                        Style.sidebarBG
    in
    Element.row
        [ Element.Events.onClick (selectClassMsg ( folderId, id ))
        , Element.width Element.fill
        , Element.padding 10
        , Element.pointer
        , Element.mouseOver [ Element.Background.color Style.hoveredItemBG ]
        , Element.Background.color bgColor
        ]
        [ fileIcon 50
        , Element.text name
        ]


fileIcon : Int -> Element msg
fileIcon width =
    Element.el [ Element.width <| Element.px width, Element.Font.center ] (Element.text ":")
