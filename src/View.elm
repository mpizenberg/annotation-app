module View exposing (view)

import Element exposing (Element, below, el, empty, node, span)
import Element.Attributes as Attributes exposing (Length, alignRight, center, fill, px, verticalCenter)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy exposing (lazy2)
import Icons
import Json.Decode as Decode
import Packages.Button as Button exposing (Button)
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Tool exposing (Tool)
import Types exposing (..)
import View.ImageAnnotations


view : Model -> Html Msg
view model =
    responsiveLayout model
        |> Element.layout Style.sheet


responsiveLayout : Model -> Element Style Style.ColorVariations Msg
responsiveLayout model =
    let
        hasAnnotation =
            -- (model.point /= Nothing)
            --     || (model.stroke /= Nothing)
            --     || (model.bbox /= Nothing)
            --     || (model.outline /= NoOutline)
            --     || (model.contour /= NoContour)
            False

        actionBarParameters =
            { device = model.device
            , size = model.layout.actionBarSize
            , canClearAnnotations = hasAnnotation
            , hasImage = model.image /= Nothing
            , toolsData = model.toolsData
            }

        toolsDataList =
            Zipper.getAll model.toolsData
                |> List.drop 1
    in
    Element.column Style.None
        [ Attributes.height fill ]
        [ deviceActionBar actionBarParameters
        , View.ImageAnnotations.imageViewer model.viewer model.image toolsDataList
        ]


type alias ActionBarParameters =
    { device : Device
    , size : ( Float, Float )
    , canClearAnnotations : Bool
    , hasImage : Bool
    , toolsData : Zipper Tool.Data
    }


deviceActionBar : ActionBarParameters -> Element Style Style.ColorVariations Msg
deviceActionBar param =
    let
        ( width, height ) =
            param.size

        filler =
            el Style.None [ Attributes.width fill, Attributes.height (px height) ] empty

        selectedToolButton : Tool.Data -> Element Style Style.ColorVariations Msg
        selectedToolButton =
            toolButton height True

        unselectedToolButton : Tool.Data -> Element Style Style.ColorVariations Msg
        unselectedToolButton =
            toolButton height False

        toolButtons =
            List.concat
                [ Zipper.getL param.toolsData
                    |> List.map unselectedToolButton
                , [ selectedToolButton (Zipper.getC param.toolsData) ]
                , Zipper.getR param.toolsData
                    |> List.map unselectedToolButton
                ]

        actionButtons =
            [ actionButton height param.canClearAnnotations ClearAnnotations Icons.trash2
            , loadFileInput height Icons.image
            ]

        zoomActions =
            [ actionButton height True (ZoomMsg ZoomIn) Icons.zoomIn
            , actionButton height True (ZoomMsg ZoomOut) Icons.zoomOut
            , actionButton height param.hasImage (ZoomMsg ZoomFit) Icons.zoomFit
            ]
    in
    case ( param.device.kind, param.device.orientation ) of
        ( Device.Phone, Device.Portrait ) ->
            (toolButtons ++ filler :: actionButtons)
                |> Element.row Style.None []
                |> below [ el Style.None [ alignRight ] (Element.row Style.None [] zoomActions) ]

        ( Device.Tablet, Device.Portrait ) ->
            (toolButtons ++ filler :: actionButtons)
                |> Element.row Style.None []
                |> below [ el Style.None [ alignRight ] (Element.row Style.None [] zoomActions) ]

        _ ->
            (toolButtons ++ filler :: actionButtons ++ filler :: zoomActions)
                |> Element.row Style.None []


actionButton : Float -> Bool -> Msg -> List (Svg Msg) -> Element Style Style.ColorVariations Msg
actionButton size clickable sendMsg innerSvg =
    Button.view
        { actionability =
            if clickable then
                Button.Abled Button.Inactive
            else
                Button.Disabled
        , action = Pointer.onDown (always sendMsg) |> Attributes.toAttr
        , innerElement = Element.html (lazy2 Icons.sized (0.6 * size) innerSvg)
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if clickable then
                Style.Button Style.Abled
            else
                Style.Button Style.Disabled
        , otherAttributes = []
        }


loadFileInput : Float -> List (Svg Msg) -> Element Style Style.ColorVariations Msg
loadFileInput size innerSvg =
    let
        invisibleInput =
            Html.input
                [ Html.Attributes.id "file-input"
                , Html.Attributes.type_ "file"
                , Html.Attributes.style [ ( "display", "none" ) ]
                , loadFileEvent
                ]
                []

        labelButton =
            Button.view
                { actionability = Button.Abled Button.Inactive
                , action = Html.Attributes.for "file-input" |> Attributes.toAttr
                , innerElement = Element.html (lazy2 Icons.sized (0.6 * size) innerSvg)
                , innerStyle = Style.None
                , size = ( size, size )
                , outerStyle = Style.Button Style.Abled
                , otherAttributes = []
                }
    in
    Element.row Style.None [] [ Element.html invisibleInput, node "label" labelButton ]


loadFileEvent : Html.Attribute Msg
loadFileEvent =
    Decode.at [ "target", "files", "0" ] Decode.value
        |> Decode.map LoadImageFile
        |> Html.Events.onWithOptions "change" stopAndPrevent


stopAndPrevent : Html.Events.Options
stopAndPrevent =
    { stopPropagation = True
    , preventDefault = True
    }



-- toolDropdown : Float -> Tool -> Tool -> Bool -> Element Style Style.ColorVariations Msg
-- toolDropdown size currentTool currentDropdownTool toolDropdownOpen =
--     let
--         downTools =
--             Tool.allAnnotationTools
--                 |> List.filter ((/=) currentDropdownTool)
--                 |> List.map (toolButton size currentTool)
--                 |> Element.column Style.None []
--                 |> List.singleton
--     in
--     el Style.None [] (toolButton size currentTool currentDropdownTool)
--         |> below
--             (if toolDropdownOpen then
--                 downTools
--              else
--                 []
--             )
--
--
-- toolButtonBis : Float -> Bool -> ToolBis -> Int -> Element Style Style.ColorVariations Msg
-- toolButtonBis size isSelected tool toolId =
--     Button.view
--         { actionability =
--             if isSelected then
--                 Button.Abled Button.Active
--             else
--                 Button.Abled Button.Inactive
--         , action = Pointer.onDown (always <| SelectTool ( Just toolId, tool )) |> Attributes.toAttr
--         , innerElement = Tool.toolSvg (0.6 * size) tool
--         , innerStyle = Style.None
--         , size = ( size, size )
--         , outerStyle =
--             if isSelected then
--                 Style.Button Style.Selected
--             else
--                 Style.Button Style.Abled
--         , otherAttributes = []
--         }


toolButton : Float -> Bool -> Tool.Data -> Element Style Style.ColorVariations Msg
toolButton size isSelected toolData =
    Button.view
        { actionability =
            if isSelected then
                Button.Abled Button.Active
            else
                Button.Abled Button.Inactive
        , action = Pointer.onDown (always <| SelectTool toolData.id) |> Attributes.toAttr
        , innerElement = Tool.svgElement (0.6 * size) toolData
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if isSelected then
                Style.Button Style.Selected
            else
                Style.Button Style.Abled
        , otherAttributes = []
        }
