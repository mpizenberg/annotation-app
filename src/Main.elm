-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Main exposing (..)

import Annotation exposing (Annotations, DragState(..), PointerMsg(..), Position)
import Annotation.Viewer as Viewer exposing (Viewer)
import Control
import Html exposing (Html)
import Image exposing (Image)
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper
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

        SelectTool toolId ->
            ( { model | toolsData = Types.selectTool toolId model.toolsData }
            , Cmd.none
            )

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
            let
                toolData =
                    Zipper.getC model.toolsData

                newToolData =
                    { toolData | tool = Tool.removeLatestAnnotation toolData.tool }
            in
            ( { model | toolsData = Zipper.setC newToolData model.toolsData }, Cmd.none )

        LoadImageFile jsValue ->
            ( model, Ports.loadImageFile jsValue )

        ImageLoaded ( src, width, height ) ->
            ( resetImage (Image src width height) model, Cmd.none )

        LoadConfigFile jsValue ->
            ( model, Ports.loadConfigFile jsValue )

        ConfigLoaded configString ->
            ( { model | toolsData = Tool.fromConfigString configString }, Cmd.none )


resetImage : Image -> Model -> Model
resetImage image model =
    let
        initialModel =
            Types.init model.device.size
    in
    { initialModel | image = Just image }
        |> resizeViewer model.viewer.size


resizeViewer : ( Float, Float ) -> Model -> Model
resizeViewer size model =
    case model.image of
        Nothing ->
            model

        Just image ->
            { model | viewer = Viewer.setSize size model.viewer |> Viewer.fitImage 0.8 image }


updateWithPointer : Annotation.PointerMsg -> Model -> Model
updateWithPointer pointerMsg model =
    let
        toolData =
            Zipper.getC model.toolsData
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
                    Tool.updateData (Viewer.positionIn model.viewer) pointerMsg model.dragState toolData
            in
            { model | toolsData = Zipper.setC newToolData model.toolsData, dragState = newDragState }


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
