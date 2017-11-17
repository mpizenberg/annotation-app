module View exposing (view)

import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Button exposing (Button)
import Device exposing (Device)
import Element exposing (Element, below, el, empty, span)
import Element.Attributes as Attributes exposing (Length, alignRight, center, fill, px, verticalCenter)
import Html exposing (Html)
import Html.Attributes
import Html.Lazy exposing (lazy2)
import Icons
import Pointer
import StyleSheet as Style exposing (Style)
import Svg exposing (Svg)
import Tool exposing (Tool)
import Types exposing (Model, Msg(..), PointerMsg(..), Position)


view : Model -> Html Msg
view model =
    responsiveLayout model
        |> Element.layout Style.sheet


responsiveLayout : Model -> Element Style variation Msg
responsiveLayout model =
    let
        ( actionBarWidth, actionBarHeight ) =
            ( model.device.size.width |> toFloat
            , deviceActionBarHeight model.device |> toFloat
            )

        ( viewerWidth, viewerHeight ) =
            ( model.device.size.width |> toFloat
            , max 0 (toFloat model.device.size.height - actionBarHeight)
            )

        actionBar =
            deviceActionBar model.device model.tool model.currentDropdownTool model.toolDropdownOpen ( actionBarWidth, actionBarHeight )
    in
    Element.column Style.None
        [ Attributes.height fill ]
        [ actionBar
        , imageViewer model.viewer model.bbox
        ]


deviceActionBarHeight : Device -> Int
deviceActionBarHeight device =
    case ( device.kind, device.orientation ) of
        ( Device.Phone, Device.Portrait ) ->
            device.size.width // 7

        ( Device.Phone, Device.Landscape ) ->
            device.size.width // 13

        ( Device.Tablet, Device.Portrait ) ->
            device.size.width // 10

        ( Device.Tablet, Device.Landscape ) ->
            device.size.width // 16

        _ ->
            min 72 (device.size.width // 16)


deviceActionBar : Device -> Tool -> Tool -> Bool -> ( Float, Float ) -> Element Style variation Msg
deviceActionBar device currentTool currentDropdownTool toolDropdownOpen ( width, height ) =
    let
        filler =
            el Style.None [ Attributes.width fill, Attributes.height (px height) ] empty

        toolButtons =
            case device.kind of
                Device.Phone ->
                    [ toolButton height currentTool Tool.Move
                    , toolDropdown height currentTool currentDropdownTool toolDropdownOpen
                    , actionButton height True ToggleToolDropdown Icons.moreVertical
                    ]

                _ ->
                    (Tool.Move :: Tool.allAnnotationTools)
                        |> List.map (toolButton height currentTool)

        actionButtons =
            [ actionButton height False NoOp Icons.rotateCcw
            , actionButton height True NoOp Icons.trash2
            , actionButton height True NoOp Icons.image
            ]

        zoomActions =
            [ actionButton height True NoOp Icons.zoomIn
            , actionButton height True NoOp Icons.zoomOut
            , actionButton height True NoOp Icons.zoomFit
            ]
    in
    case ( device.kind, device.orientation ) of
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


actionButton : Float -> Bool -> Msg -> List (Svg Msg) -> Element Style v Msg
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
        , outerStyle = Style.Button (not clickable)
        , otherAttributes = [ Attributes.attribute "elm-pep" "true" ]
        }


toolDropdown : Float -> Tool -> Tool -> Bool -> Element Style variation Msg
toolDropdown size currentTool currentDropdownTool toolDropdownOpen =
    let
        downTools =
            Tool.allAnnotationTools
                |> List.filter ((/=) currentDropdownTool)
                |> List.map (toolButton size currentTool)
                |> Element.column Style.None []
                |> List.singleton
    in
    el Style.None [] (toolButton size currentTool currentDropdownTool)
        |> below
            (if toolDropdownOpen then
                downTools
             else
                []
            )


toolButton : Float -> Tool -> Tool -> Element Style variation Msg
toolButton size currentTool tool =
    Button.view
        { actionability =
            Button.Abled
                (if tool == currentTool then
                    Button.Active
                 else
                    Button.Inactive
                )
        , action = Pointer.onDown (always <| SelectTool tool) |> Attributes.toAttr
        , innerElement = Tool.svgElement (0.6 * size) tool
        , innerStyle = Style.None
        , size = ( size, size )
        , outerStyle =
            if tool == currentTool then
                Style.CurrentTool
            else
                Style.Button False
        , otherAttributes = [ Attributes.attribute "elm-pep" "true" ]
        }


imageViewer : Viewer -> Maybe BoundingBox -> Element Style variation Msg
imageViewer viewer maybeBBox =
    let
        attributes =
            [ Html.Attributes.style [ ( "height", "100%" ) ]
            , Html.Attributes.attribute "elm-pep" "true"
            , Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt >> PointerMsg)
            , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt >> PointerMsg)
            , Pointer.onUp (.pointer >> .offsetPos >> PointerUpAt >> PointerMsg)
            ]
    in
    Viewer.viewInWithDetails attributes viewer (viewBBox maybeBBox)
        |> Element.html
        |> el Style.Viewer [ Attributes.height fill ]


viewBBox : Maybe BoundingBox -> Svg msg
viewBBox maybeBBox =
    maybeBBox
        |> Maybe.map Svg.boundingBox
        |> Maybe.withDefault (Svg.text "No bounding box")
