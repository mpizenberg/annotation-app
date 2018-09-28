module Packages.FileSystem exposing
    ( FileSystem, Folder, File
    , initWithFolder
    , root, currentFolder, updateCurrentFolder
    , subfolders
    , findFolderWithId, findAccessibleFolderWithId, findFolderWithIdPath
    , insertFile, insertFolder, insertSubsystem
    , stub
    )

{-| Helper module to manipulate hierarchical structures
similar to file systems.

@docs FileSystem, Folder, File

@docs initWithFolder

@docs root, currentFolder, updateCurrentFolder

@docs subfolders

@docs findFolderWithId, findAccessibleFolderWithId, findFolderWithIdPath

@docs insertFile, insertFolder, insertSubsystem

-}

import Tree exposing (Tree)
import Tree.Zipper


{-| A simple folder type.
-}
type alias Folder =
    { id : Int
    , name : String
    , open : Bool
    , files : List File
    }


{-| A simple file type.
-}
type alias File =
    { id : Int
    , name : String
    }


{-| A file system type, based on a tree zipper.
It always has a current focused folder.
-}
type alias FileSystem =
    Tree.Zipper.Zipper Folder


{-| -}
initWithFolder : Folder -> FileSystem
initWithFolder folder =
    Tree.Zipper.fromTree (Tree.singleton folder)


folderRoot =
    Folder 0 "root" True []


folder1 =
    Folder 1 "one" True [ File 0 "0" ]


folder2 =
    Folder 2 "two" False [ File 14 "file at root", File 42 "Second file" ]


folder3 =
    Folder 3 "three" True [ File 1 "1", File 2 "2" ]


stub =
    Tree.Zipper.fromTree <|
        Tree.tree folderRoot
            [ Tree.singleton folder1
            , Tree.tree folder2
                [ Tree.singleton folder3
                ]
            ]


{-| Go to the root of the file system.
-}
root : FileSystem -> FileSystem
root =
    Tree.Zipper.root


{-| Retrieve the current focused folder.
-}
currentFolder : FileSystem -> Folder
currentFolder =
    Tree.Zipper.label


{-| Update the current focused folder.
-}
updateCurrentFolder : (Folder -> Folder) -> FileSystem -> FileSystem
updateCurrentFolder =
    Tree.Zipper.mapLabel


{-| -}
insertFile : File -> FileSystem -> FileSystem
insertFile file =
    updateCurrentFolder (\folder -> { folder | files = file :: folder.files })


{-| -}
insertFolder : Folder -> FileSystem -> FileSystem
insertFolder folder fileSystem =
    let
        folderNode =
            Tree.singleton folder
    in
    fileSystem
        |> Tree.Zipper.mapTree (Tree.prependChild folderNode)


{-| -}
insertSubsystem : FileSystem -> FileSystem -> FileSystem
insertSubsystem subsystem fileSystem =
    let
        subtree =
            Tree.Zipper.tree subsystem
    in
    fileSystem
        |> Tree.Zipper.mapTree (Tree.prependChild subtree)


{-| The list of sub folders (one level deep).
Each subfolder is returned as the current focused folder of a file system.
This makes it easier to iterate through the file system.
-}
subfolders : FileSystem -> List FileSystem
subfolders fileSystem =
    case Tree.Zipper.lastChild fileSystem of
        Nothing ->
            []

        Just child ->
            sibblingsAcc child [ child ]


sibblingsAcc : FileSystem -> List FileSystem -> List FileSystem
sibblingsAcc fileSystem acc =
    case Tree.Zipper.previousSibling fileSystem of
        Nothing ->
            acc

        Just sibbling ->
            sibblingsAcc sibbling (sibbling :: acc)


{-| Search anywhere for a folder with given id.
If found, return the file system with focus on this folder.
-}
findFolderWithId : Int -> FileSystem -> Maybe FileSystem
findFolderWithId id =
    Tree.Zipper.findFromRoot (folderIdIs id)


folderIdIs : Int -> Folder -> Bool
folderIdIs id folder =
    folder.id == id


{-| Search an "accessible" folder with given id.
Accessible here means that the path to this folder from root
contains only open folders.
This folder may be closed though.

If found, return the file system with focus on this folder.

-}
findAccessibleFolderWithId : Int -> FileSystem -> Maybe FileSystem
findAccessibleFolderWithId id fileSystem =
    findInAccessibleSiblings id (root fileSystem)


findInAccessibleSiblings : Int -> FileSystem -> Maybe FileSystem
findInAccessibleSiblings id fileSystem =
    if .id (currentFolder fileSystem) == id then
        Just fileSystem

    else
        case Tree.Zipper.nextSibling fileSystem of
            Nothing ->
                findInAccessibleChild id fileSystem

            Just sibling ->
                findInAccessibleSiblings id sibling


findInAccessibleChild : Int -> FileSystem -> Maybe FileSystem
findInAccessibleChild id fileSystem =
    if .open (currentFolder fileSystem) then
        Tree.Zipper.firstChild fileSystem
            |> Maybe.andThen (findInAccessibleSiblings id)

    else
        Nothing


{-| Search for a folder at the given absolute id path.
If found, return the file system with focus on this folder.
-}
findFolderWithIdPath : List Int -> FileSystem -> Maybe FileSystem
findFolderWithIdPath path fileSystem =
    case path of
        [] ->
            Nothing

        id :: subpath ->
            findPathInSibblings id subpath (root fileSystem)


findPathInSibblings : Int -> List Int -> FileSystem -> Maybe FileSystem
findPathInSibblings id subpath fileSystem =
    if .id (currentFolder fileSystem) == id then
        case subpath of
            [] ->
                Just fileSystem

            nextId :: nextSubpath ->
                findPathInChild nextId nextSubpath fileSystem

    else
        -- Explicit "case" instead of "Maybe.map" to have clear tail call recursion.
        case Tree.Zipper.nextSibling fileSystem of
            Nothing ->
                Nothing

            Just sibling ->
                findPathInSibblings id subpath sibling


findPathInChild : Int -> List Int -> FileSystem -> Maybe FileSystem
findPathInChild id subpath fileSystem =
    Tree.Zipper.firstChild fileSystem
        |> Maybe.andThen (findPathInSibblings id subpath)
