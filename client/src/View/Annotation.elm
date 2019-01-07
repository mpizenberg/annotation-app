module View.Annotation exposing (view)

import Annotation.Color as Color exposing (Color)
import Annotation.Svg
import Data.Annotation as Annotation exposing (Annotation(..))
import Svg exposing (Svg)


view : Annotation -> Svg msg
view annotation =
    case annotation of
        Point point ->
            Annotation.Svg.pointStyled Nothing (Just fillColor) 50 point

        BBox rectangle ->
            Annotation.Svg.rectangleStyled (Just lineStyle) (Just fillColor) rectangle

        Line line ->
            Annotation.Svg.lineStyled (Just lineStyle) line

        UnfinishedOutline line ->
            Annotation.Svg.lineStyled (Just lineStyle) line

        Outline line ->
            Annotation.Svg.polygonStyled (Just lineStyle) (Just fillColor) line

        UnfinishedPolygon line ->
            Annotation.Svg.lineStyled (Just lineStyle) line

        Polygon line ->
            Annotation.Svg.polygonStyled (Just lineStyle) (Just fillColor) line


fillColor : Color
fillColor =
    Color.turquoise


lineStyle : Annotation.Svg.LineStyle
lineStyle =
    { width = 10
    , color = Color.green
    }
