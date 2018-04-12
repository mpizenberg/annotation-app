module View.Main
    exposing
        ( Parameters
        , markHasAnnotation
        , markHasImage
        , pageLayout
        , updateLayout
        , viewAll
        , viewConfig
        , viewImages
        , viewNothing
        )

import Annotation.Viewer as Viewer exposing (Viewer)
import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage)
import Data.RawImage as RawImage exposing (RawImage)
import Data.Tool as Tool exposing (Tool)
import Element exposing (Element)
import Element.Attributes as Attributes exposing (alignLeft, alignRight, fill, height, paddingTop)
import Html exposing (Html)
import Packages.Device as Device exposing (Device)
import Packages.StaticTreeMap as StaticTreeMap exposing (StaticTreeMap)
import Packages.Zipper as Zipper exposing (Zipper)
import StyleSheet as Style exposing (ColorVariations, Style)
import View.ActionBar as ActionBar
import View.AnnotationsArea as AnnotationsArea
import View.ClassesSideBar as ClassesSideBar
import View.DatasetSideBar as DatasetSideBar


-- TYPES #############################################################


type alias Parameters msg =
    { device : Device
    , actionBar : ActionBar.Parameters msg
    , annotationsArea : AnnotationsArea.Parameters msg
    , selectClassMsg : Int -> msg
    , selectImageMsg : Int -> msg
    }



-- FUNCTIONS #########################################################


viewNothing : Parameters msg -> Html msg
viewNothing params =
    Element.layout Style.sheet <|
        ActionBar.emptyView params.actionBar


viewImages : Parameters msg -> Viewer -> Zipper RawImage -> Html msg
viewImages params viewer images =
    Element.layout Style.sheet <|
        Element.column Style.None
            [ Attributes.height fill ]
            [ ActionBar.emptyView params.actionBar
                |> Element.below [ datasetRawSideBar params.selectImageMsg images ]
            , AnnotationsArea.viewImageOnly viewer (Zipper.getC images)
            ]


viewConfig : Parameters msg -> Zipper Tool -> { selected : Int, all : StaticTreeMap String } -> Html msg
viewConfig params tools classes =
    Element.layout Style.sheet <|
        Element.el Style.None
            [ Attributes.height fill ]
            (ActionBar.view params.actionBar tools
                |> Element.below [ classesSideBar params.selectClassMsg classes ]
            )


viewAll : Parameters msg -> Zipper Tool -> Viewer -> { selected : Int, all : StaticTreeMap String } -> Zipper AnnotatedImage -> Html msg
viewAll params tools viewer ({ selected, all } as classes) annotatedImages =
    Element.layout Style.sheet <|
        Element.column Style.None
            [ Attributes.height fill ]
            [ ActionBar.view params.actionBar tools
                |> Element.below [ classesSideBar params.selectClassMsg classes ]
                |> Element.below [ datasetAnnotatedSideBar params.selectImageMsg annotatedImages ]
            , AnnotationsArea.view params.annotationsArea viewer selected (Zipper.getC annotatedImages)
            ]



-- sub views helpers


datasetRawSideBar : (Int -> msg) -> Zipper RawImage -> Element Style var msg
datasetRawSideBar selectImageMsg images =
    DatasetSideBar.viewRaw selectImageMsg images
        |> Element.el Style.ClassesSidebar [ alignRight, paddingTop 10 ]


datasetAnnotatedSideBar : (Int -> msg) -> Zipper AnnotatedImage -> Element Style var msg
datasetAnnotatedSideBar selectImageMsg images =
    DatasetSideBar.viewAnnotated selectImageMsg images
        |> Element.el Style.ClassesSidebar [ alignRight, paddingTop 10 ]


classesSideBar : (Int -> msg) -> { selected : Int, all : StaticTreeMap String } -> Element Style var msg
classesSideBar selectClassMsg classes =
    ClassesSideBar.view selectClassMsg classes
        |> Element.el Style.ClassesSidebar [ alignLeft, paddingTop 10 ]



-- update helpers


markHasImage : Parameters msg -> Parameters msg
markHasImage ({ actionBar } as params) =
    { params | actionBar = { actionBar | hasImage = True } }


markHasAnnotation : Bool -> Parameters msg -> Parameters msg
markHasAnnotation hasAnnotations ({ actionBar } as params) =
    if actionBar.hasAnnotations == hasAnnotations then
        params
    else
        { params | actionBar = { actionBar | hasAnnotations = hasAnnotations } }


pageLayout : Device -> { actionBarSize : ( Float, Float ), viewerSize : ( Float, Float ) }
pageLayout device =
    let
        ( barWidth, barHeight ) =
            ( device.size.width |> toFloat
            , ActionBar.responsiveHeight device |> toFloat
            )

        ( viewerWidth, viewerHeight ) =
            ( device.size.width |> toFloat
            , max 0 (toFloat device.size.height - barHeight)
            )
    in
    { actionBarSize = ( barWidth, barHeight )
    , viewerSize = ( viewerWidth, viewerHeight )
    }


updateLayout : Device.Size -> Parameters msg -> ( Parameters msg, ( Float, Float ) )
updateLayout size params =
    let
        device =
            Device.classify size

        layout =
            pageLayout device

        updateSize newSize parameters =
            { parameters | size = newSize }

        actionBar =
            updateSize layout.actionBarSize params.actionBar

        annotationsArea =
            updateSize layout.viewerSize params.annotationsArea
    in
    ( { params | device = device, actionBar = actionBar, annotationsArea = annotationsArea }
    , layout.viewerSize
    )
