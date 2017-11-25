-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


port module Main exposing (..)

import Annotation.Geometry.BoundingBox as BBox
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Stroke as Stroke
import Annotation.Viewer as Viewer
import Control
import Device exposing (Device)
import Html exposing (Html)
import Image exposing (Image)
import Json.Encode as Encode
import Tool exposing (Tool)
import Types exposing (..)
import View


main : Program Device.Size Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL #############################################################


port resizes : (Device.Size -> msg) -> Sub msg


port loadImageFile : Encode.Value -> Cmd msg


port imageLoaded : (( String, Int, Int ) -> msg) -> Sub msg


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

        SelectTool tool ->
            ( { model
                | tool = tool
                , toolDropdownOpen = False
                , currentDropdownTool =
                    if tool /= Tool.Move then
                        tool
                    else
                        model.currentDropdownTool
              }
            , Cmd.none
            )

        ToggleToolDropdown ->
            ( { model | toolDropdownOpen = not model.toolDropdownOpen }
            , Cmd.none
            )

        PointerMsg pointerMsg ->
            ( updateWithPointer pointerMsg model, Cmd.none )

        MoveThrottle throttleMsg ->
            Control.update (\s -> { model | moveThrottleState = s }) model.moveThrottleState throttleMsg

        ZoomMsg zoomMsg ->
            ( updateZoom zoomMsg model, Cmd.none )

        LoadImageFile jsValue ->
            ( model, loadImageFile jsValue )

        ImageLoaded ( src, width, height ) ->
            ( resetImage (Image src width height) model, Cmd.none )


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


updateWithPointer : PointerMsg -> Model -> Model
updateWithPointer pointerMsg model =
    case ( pointerMsg, model.tool, model.dragState ) of
        ( PointerDownAt pos, Tool.Move, _ ) ->
            { model | dragState = DraggingFrom pos }

        ( PointerMoveAt ( x, y ), Tool.Move, DraggingFrom ( ox, oy ) ) ->
            { model
                | dragState = DraggingFrom ( x, y )
                , viewer = Viewer.grabMove ( x - ox, y - oy ) model.viewer
            }

        ( PointerUpAt _, Tool.Move, _ ) ->
            { model | dragState = NoDrag }

        ( PointerDownAt pos, Tool.BBox, _ ) ->
            { model
                | dragState = DraggingFrom (Viewer.positionIn model.viewer pos)
                , bbox = Nothing
            }

        ( PointerMoveAt pos, Tool.BBox, DraggingFrom startPos ) ->
            let
                ( startPoint, point ) =
                    ( Point.fromCoordinates startPos
                    , Point.fromCoordinates (Viewer.positionIn model.viewer pos)
                    )
            in
            { model | bbox = Just (BBox.fromPair ( startPoint, point )) }

        ( PointerUpAt _, Tool.BBox, _ ) ->
            { model | dragState = NoDrag }

        ( _, Tool.Contour, _ ) ->
            updateContour pointerMsg model

        _ ->
            model


updateContour : PointerMsg -> Model -> Model
updateContour pointerMsg model =
    case ( pointerMsg, model.dragState, model.contour ) of
        ( PointerDownAt pos, NoDrag, _ ) ->
            let
                scaledPos =
                    Viewer.positionIn model.viewer pos

                dragState =
                    DraggingFrom scaledPos

                point =
                    Point.fromCoordinates scaledPos

                ( startPos, newStroke ) =
                    case model.contour of
                        DrawingStartedAt scaledStartPos stroke ->
                            ( scaledStartPos, Stroke.addPoint point stroke )

                        _ ->
                            ( scaledPos, Stroke.fromPoints [ point ] )

                contour =
                    DrawingStartedAt startPos newStroke
            in
            { model | dragState = dragState, contour = contour }

        ( PointerMoveAt pos, DraggingFrom _, DrawingStartedAt scaledStartPos stroke ) ->
            let
                scaledPos =
                    Viewer.positionIn model.viewer pos

                point =
                    Point.fromCoordinates scaledPos

                ( startPos, newStroke ) =
                    case Stroke.points stroke of
                        _ :: [] ->
                            ( scaledPos, Stroke.fromPoints [ point ] )

                        _ :: rest ->
                            ( scaledStartPos, Stroke.fromPoints (point :: rest) )

                        _ ->
                            ( scaledStartPos, stroke )

                contour =
                    DrawingStartedAt startPos newStroke
            in
            { model | contour = contour }

        ( PointerUpAt pos, DraggingFrom _, DrawingStartedAt scaledStartPos stroke ) ->
            case Stroke.points stroke of
                startPoint :: [] ->
                    { model | dragState = NoDrag }

                endPoint :: rest ->
                    let
                        scaledEndPos =
                            Viewer.positionIn model.viewer pos

                        distance ( x1, x2 ) ( y1, y2 ) =
                            abs (x1 - y1) + abs (x2 - y2)
                    in
                    if distance scaledEndPos scaledStartPos > 10 then
                        { model | dragState = NoDrag }
                    else
                        { model
                            | dragState = NoDrag
                            , contour = Ended (Stroke.close <| Stroke.fromPoints rest)
                        }

                _ ->
                    { model | dragState = NoDrag }

        _ ->
            model


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
        [ resizes WindowResizesMsg
        , imageLoaded ImageLoaded
        ]



-- VIEW ##############################################################


view : Model -> Html Msg
view =
    View.view
