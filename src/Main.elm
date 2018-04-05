-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (..)

import Annotation exposing (Annotations, DragState(..), PointerMsg(..), Position)
import Annotation.Viewer as Viewer exposing (Viewer)
import Class
import Control
import Html exposing (Html)
import Image exposing (Image)
import Json.Decode as Decode exposing (Decoder, Value)
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Ports
import Tool exposing (Tool)
import Types exposing (..)
import View.Main


main : Program Device.Size Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = View.Main.view
        , subscriptions = subscriptions
        }



-- MODEL #############################################################


init : Device.Size -> ( Model, Cmd Msg )
init sizeFlag =
    ( Types.init sizeFlag, Cmd.none )



-- UPDATE ############################################################


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResizesMsg size ->
            let
                device =
                    Device.classify size

                layout =
                    Types.pageLayout device

                viewer =
                    Viewer.setSize layout.viewerSize model.viewer
            in
            ( { model | device = device, layout = layout, viewer = viewer }
            , Cmd.none
            )

        SelectImage imageId ->
            ( { model | imagesData = moveToId imageId model.imagesData }
                |> resizeViewer model.viewer.size
            , Cmd.none
            )

        SelectClass classKey ->
            let
                classesData =
                    model.classesData
            in
            ( { model | classesData = { classesData | selectedKey = classKey } }
            , Cmd.none
            )

        SelectTool toolId ->
            case Zipper.getC model.imagesData of
                Loaded imageId imageName image toolsData ->
                    let
                        newImageData =
                            Loaded imageId imageName image (Types.selectTool toolId toolsData)

                        newModel =
                            { model | imagesData = Zipper.setC newImageData model.imagesData }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleToolDropdown ->
            -- ( { model | toolDropdownOpen = not model.toolDropdownOpen }
            ( model
            , Cmd.none
            )

        PointerMsg pointerMsg ->
            ( updateWithPointer pointerMsg model, Cmd.none )

        MoveThrottle throttleMsg ->
            Control.update (\s -> { model | moveThrottleState = s }) model.moveThrottleState throttleMsg

        ZoomMsg zoomMsg ->
            ( updateZoom zoomMsg model, Cmd.none )

        ClearAnnotations ->
            case Zipper.getC model.imagesData of
                Loaded id imageName image toolsData ->
                    let
                        toolData =
                            Zipper.getC toolsData

                        newToolData =
                            { toolData | tool = Tool.removeLatestAnnotation toolData.tool }

                        newImageData =
                            Loaded id imageName image (Zipper.setC newToolData toolsData)

                        newModel =
                            { model | imagesData = Zipper.setC newImageData model.imagesData }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadImages files ->
            let
                ( newImagesData, cmds ) =
                    prepareLoading files model.imagesData
            in
            ( { model | imagesData = newImagesData }
            , cmds
            )

        ImageLoaded ( id, src, width, height ) ->
            let
                toolsData =
                    Tool.fromConfig model.config

                newImagesData =
                    setLoadedImage id src ( width, height ) toolsData model.imagesData

                loadedCurrentImage =
                    case Zipper.getC newImagesData of
                        Loaded currentId _ _ _ ->
                            True

                        _ ->
                            False

                newModel =
                    { model | imagesData = newImagesData }
            in
            if loadedCurrentImage then
                ( resizeViewer newModel.viewer.size newModel, Cmd.none )
            else
                ( newModel, Cmd.none )

        LoadConfigFile jsValue ->
            ( model, Ports.loadConfigFile jsValue )

        ConfigLoaded configString ->
            let
                config =
                    Decode.decodeString Annotation.configDecoder configString
                        |> Result.withDefault Annotation.emptyConfig

                toolsData =
                    Tool.fromConfig config
            in
            ( { model
                | config = config
                , classesData = { selectedKey = 0, classes = Class.fromConfig config }
                , imagesData = resetImagesDataWithTools toolsData model.imagesData
              }
            , Cmd.none
            )


moveToId : Int -> Zipper ImageData -> Zipper ImageData
moveToId id zipper =
    case Zipper.getC zipper of
        Loading itemId _ ->
            if itemId == id then
                zipper
            else if itemId < id then
                Zipper.goR zipper
                    |> moveToId id
            else
                Zipper.goL zipper
                    |> moveToId id

        _ ->
            zipper


prepareLoading : List ( String, Value ) -> Zipper ImageData -> ( Zipper ImageData, Cmd Msg )
prepareLoading images imagesData =
    List.foldl addPrepareImage ( Zipper.goEnd imagesData, Cmd.none ) images


addPrepareImage : ( String, Value ) -> ( Zipper ImageData, Cmd Msg ) -> ( Zipper ImageData, Cmd Msg )
addPrepareImage ( name, value ) ( zipper, cmds ) =
    case Zipper.getC zipper of
        EmptyImageData ->
            ( Zipper.init [] (Loading 0 name) [], Ports.loadImageFile ( 0, value ) )

        Loading id _ ->
            ( Zipper.insertR (Loading (id + 1) name) zipper |> Zipper.goR
            , Cmd.batch [ Ports.loadImageFile ( id + 1, value ), cmds ]
            )

        Loaded id _ _ _ ->
            ( Zipper.insertR (Loading (id + 1) name) zipper |> Zipper.goR
            , Cmd.batch [ Ports.loadImageFile ( id + 1, value ), cmds ]
            )


markLoaded : String -> ( Int, Int ) -> Zipper Tool.Data -> ImageData -> ImageData
markLoaded src ( width, height ) toolsData imagesData =
    case imagesData of
        Loading id imageName ->
            Loaded id imageName (Image src width height) toolsData

        _ ->
            imagesData


setLoadedImage : Int -> String -> ( Int, Int ) -> Zipper Tool.Data -> Zipper ImageData -> Zipper ImageData
setLoadedImage imageId src size toolsData imagesData =
    -- let
    --     initialModel =
    --         Types.initWithConfig model.config model.device.size
    -- in
    -- { initialModel | image = Just image }
    --     |> resizeViewer model.viewer.size
    case Zipper.getC imagesData of
        EmptyImageData ->
            imagesData

        Loading currentId _ ->
            moveToId imageId imagesData
                |> Zipper.updateC (markLoaded src size toolsData)
                |> moveToId currentId

        Loaded currentId _ _ _ ->
            moveToId imageId imagesData
                |> Zipper.updateC (markLoaded src size toolsData)
                |> moveToId currentId


resetImagesDataWithTools : Zipper Tool.Data -> Zipper ImageData -> Zipper ImageData
resetImagesDataWithTools toolsData imagesData =
    Zipper.goEnd imagesData
        |> Zipper.moveMapStart (resetImageWithTool toolsData)


resetImageWithTool : Zipper Tool.Data -> ImageData -> ImageData
resetImageWithTool toolsData imageData =
    case imageData of
        Loaded id imageName image _ ->
            Loaded id imageName image toolsData

        _ ->
            imageData


resizeViewer : ( Float, Float ) -> Model -> Model
resizeViewer size model =
    case Zipper.getC model.imagesData of
        Loaded _ _ image toolsData ->
            { model | viewer = Viewer.setSize size model.viewer |> Viewer.fitImage 0.8 image }

        _ ->
            model


updateWithPointer : Annotation.PointerMsg -> Model -> Model
updateWithPointer pointerMsg model =
    case Zipper.getC model.imagesData of
        Loaded id imageName image toolsData ->
            let
                toolData =
                    Zipper.getC toolsData
            in
            case toolData.tool of
                Tool.Move ->
                    let
                        ( newViewer, newDragState ) =
                            updateMove pointerMsg model.dragState model.viewer
                    in
                    { model | viewer = newViewer, dragState = newDragState }

                _ ->
                    let
                        ( newToolData, newDragState ) =
                            Tool.updateData
                                model.classesData.selectedKey
                                (Viewer.positionIn model.viewer)
                                pointerMsg
                                model.dragState
                                toolData

                        newToolsData =
                            Zipper.setC newToolData toolsData

                        newImagesData =
                            Zipper.setC (Loaded id imageName image newToolsData) model.imagesData
                    in
                    { model | imagesData = newImagesData, dragState = newDragState }

        _ ->
            model


updateMove : PointerMsg -> DragState -> Viewer -> ( Viewer, DragState )
updateMove pointerMsg dragState viewer =
    case ( pointerMsg, dragState ) of
        ( PointerDownAt pos, _ ) ->
            ( viewer, DraggingFrom pos )

        ( PointerMoveAt ( x, y ), DraggingFrom ( ox, oy ) ) ->
            ( Viewer.grabMove ( x - ox, y - oy ) viewer, DraggingFrom ( x, y ) )

        ( PointerUpAt _, _ ) ->
            ( viewer, NoDrag )

        _ ->
            ( viewer, dragState )



-- case ( pointerMsg, model.tool, model.dragState ) of
--     ( PointerDownAt pos, Tool.BBox, _ ) ->
--         { model
--             | dragState = DraggingFrom (Viewer.positionIn model.viewer pos)
--             , bbox = Nothing
--         }
--
--     ( PointerMoveAt pos, Tool.BBox, DraggingFrom startPos ) ->
--         let
--             ( startPoint, point ) =
--                 ( Point.fromCoordinates startPos
--                 , Point.fromCoordinates (Viewer.positionIn model.viewer pos)
--                 )
--         in
--         { model | bbox = Just (BBox.fromPair ( startPoint, point )) }
--
--     ( PointerUpAt _, Tool.BBox, _ ) ->
--         { model | dragState = NoDrag }
--
--     ( _, Tool.Contour, _ ) ->
--         updateContour pointerMsg model
--
--     ( _, Tool.Point, _ ) ->
--         updatePoint pointerMsg model
--
--     ( _, Tool.Stroke, _ ) ->
--         updateStroke pointerMsg model
--
--     ( _, Tool.Outline, _ ) ->
--         updateOutline pointerMsg model
--     _ ->
--
--
-- updateStroke : PointerMsg -> Model -> Model
-- updateStroke pointerMsg model =
--     case ( pointerMsg, model.dragState, model.stroke ) of
--         ( PointerDownAt pos, NoDrag, _ ) ->
--             let
--                 scaledPos =
--                     Viewer.positionIn model.viewer pos
--             in
--             { model
--                 | stroke = Just (Stroke.fromPoints [ Point.fromCoordinates scaledPos ])
--                 , dragState = DraggingFrom scaledPos
--             }
--
--         ( PointerMoveAt pos, DraggingFrom _, Just stroke ) ->
--             let
--                 scaledPos =
--                     Viewer.positionIn model.viewer pos
--             in
--             { model | stroke = Just (Stroke.addPoint (Point.fromCoordinates scaledPos) stroke) }
--
--         ( PointerUpAt _, _, _ ) ->
--             { model | dragState = NoDrag }
--
--         _ ->
--             model
--
--
--
-- updateOutline : PointerMsg -> Model -> Model
-- updateOutline pointerMsg model =
--     case ( pointerMsg, model.dragState, model.outline ) of
--         ( PointerDownAt pos, NoDrag, _ ) ->
--             let
--                 scaledPos =
--                     Viewer.positionIn model.viewer pos
--
--                 dragState =
--                     DraggingFrom scaledPos
--
--                 point =
--                     Point.fromCoordinates scaledPos
--
--                 outline =
--                     DrawingOutline (Stroke.fromPoints [ point ])
--             in
--             { model | dragState = dragState, outline = outline }
--
--         ( PointerMoveAt pos, DraggingFrom _, DrawingOutline stroke ) ->
--             let
--                 scaledPos =
--                     Viewer.positionIn model.viewer pos
--
--                 point =
--                     Point.fromCoordinates scaledPos
--
--                 outline =
--                     DrawingOutline (Stroke.fromPoints <| point :: Stroke.points stroke)
--             in
--             { model | outline = outline }
--
--         ( PointerUpAt pos, DraggingFrom _, DrawingOutline stroke ) ->
--             { model | dragState = NoDrag, outline = EndedOutline (Stroke.close stroke) }
--
--         _ ->
--             model
--
--
-- updateContour : PointerMsg -> Model -> Model
-- updateContour pointerMsg model =
--     case ( pointerMsg, model.dragState, model.contour ) of
--         ( PointerDownAt pos, NoDrag, _ ) ->
--             let
--                 scaledPos =
--                     Viewer.positionIn model.viewer pos
--
--                 dragState =
--                     DraggingFrom scaledPos
--
--                 point =
--                     Point.fromCoordinates scaledPos
--
--                 ( startPos, newStroke ) =
--                     case model.contour of
--                         DrawingStartedAt scaledStartPos stroke ->
--                             ( scaledStartPos, Stroke.addPoint point stroke )
--
--                         _ ->
--                             ( scaledPos, Stroke.fromPoints [ point ] )
--
--                 contour =
--                     DrawingStartedAt startPos newStroke
--             in
--             { model | dragState = dragState, contour = contour }
--
--         ( PointerMoveAt pos, DraggingFrom _, DrawingStartedAt scaledStartPos stroke ) ->
--             let
--                 scaledPos =
--                     Viewer.positionIn model.viewer pos
--
--                 point =
--                     Point.fromCoordinates scaledPos
--
--                 ( startPos, newStroke ) =
--                     case Stroke.points stroke of
--                         _ :: [] ->
--                             ( scaledPos, Stroke.fromPoints [ point ] )
--
--                         _ :: rest ->
--                             ( scaledStartPos, Stroke.fromPoints (point :: rest) )
--
--                         _ ->
--                             ( scaledStartPos, stroke )
--
--                 contour =
--                     DrawingStartedAt startPos newStroke
--             in
--             { model | contour = contour }
--
--         ( PointerUpAt pos, DraggingFrom _, DrawingStartedAt scaledStartPos stroke ) ->
--             case Stroke.points stroke of
--                 startPoint :: [] ->
--                     { model | dragState = NoDrag }
--
--                 endPoint :: rest ->
--                     let
--                         scaledEndPos =
--                             Viewer.positionIn model.viewer pos
--
--                         distance ( x1, x2 ) ( y1, y2 ) =
--                             abs (x1 - y1) + abs (x2 - y2)
--                     in
--                     if distance scaledEndPos scaledStartPos > (30 / model.viewer.zoom) then
--                         { model | dragState = NoDrag }
--                     else
--                         { model
--                             | dragState = NoDrag
--                             , contour = Ended (Stroke.close <| Stroke.fromPoints rest)
--                         }
--
--                 _ ->
--                     { model | dragState = NoDrag }
--
--         _ ->
--             model


updateZoom : ZoomMsg -> Model -> Model
updateZoom zoomMsg model =
    case zoomMsg of
        ZoomIn ->
            { model | viewer = Viewer.setZoomCentered (1.5625 * model.viewer.zoom) model.viewer }

        ZoomOut ->
            { model | viewer = Viewer.setZoomCentered (0.64 * model.viewer.zoom) model.viewer }

        ZoomFit ->
            resizeViewer model.viewer.size model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.resizes WindowResizesMsg
        , Ports.imageLoaded ImageLoaded
        , Ports.configLoaded ConfigLoaded
        ]
