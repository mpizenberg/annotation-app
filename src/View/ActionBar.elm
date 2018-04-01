module View.ActionBar exposing (..)

import Element exposing (Element, below, el, empty, node)
import Element.Attributes as Attributes exposing (alignRight, fill, px)
import Html.Lazy exposing (lazy2)
import Packages.Button as Button exposing (Button)
import Packages.Device as Device exposing (Device)
import Packages.Zipper as Zipper exposing (Zipper)
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Tool
import Types exposing (..)
import View.Icons as Icons


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
            , Button.loadFileInput
                { msgTagger = LoadConfigFile
                , uniqueId = "config-loader"
                , innerElement = Element.html (lazy2 Icons.sized (0.6 * height) Icons.settings)
                , size = height
                , noStyle = Style.None
                , outerStyle = Style.Button Style.Abled
                }
            , Button.loadFileInput
                { msgTagger = LoadImageFile
                , uniqueId = "image-loader"
                , innerElement = Element.html (lazy2 Icons.sized (0.6 * height) Icons.image)
                , size = height
                , noStyle = Style.None
                , outerStyle = Style.Button Style.Abled
                }
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
