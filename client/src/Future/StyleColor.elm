module Future.StyleColor exposing (background, text)

import Future.Color
import Style.Color


background color =
    Style.Color.background (Future.Color.toOld color)


text color =
    Style.Color.text (Future.Color.toOld color)
