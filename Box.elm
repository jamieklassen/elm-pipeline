module Box exposing (box, height, width)

import Svg
import Svg.Attributes
    exposing
        ( dominantBaseline
        , fontFamily
        , fontSize
        , fill
        , textAnchor
        )


magicRatioOfHeightToWidthInMonospaceFonts : Float
magicRatioOfHeightToWidthInMonospaceFonts =
    0.6


width :
    { text : String
    , textSize : Int
    , padding : Int
    }
    -> Float
width { text, textSize, padding } =
    let
        glyphWidth =
            toFloat textSize * magicRatioOfHeightToWidthInMonospaceFonts

        textLength =
            toFloat <| String.length text

        floatPadding =
            toFloat padding
    in
        floatPadding + textLength * glyphWidth + floatPadding


height :
    { textSize : Int
    , padding : Int
    }
    -> Float
height { textSize, padding } =
    let
        glyphHeight =
            toFloat textSize
    in
        toFloat padding + glyphHeight + toFloat padding


box :
    { backgroundColor : String
    , textColor : String
    , text : String
    , textSize : Int
    , padding : Int
    , x : Float
    , y : Float
    }
    -> Svg.Svg msg
box { backgroundColor, textColor, text, textSize, padding, x, y } =
    let
        rectHeight =
            height { textSize = textSize, padding = padding }

        rectWidth =
            width
                { text = text
                , textSize = textSize
                , padding = padding
                }
    in
        Svg.g
            []
            [ Svg.rect
                [ fill backgroundColor
                , Svg.Attributes.height <| toString rectHeight
                , Svg.Attributes.width <| toString rectWidth
                , Svg.Attributes.x <| toString x
                , Svg.Attributes.y <| toString y
                ]
                []
            , Svg.text_
                [ fill textColor
                , Svg.Attributes.x <| toString (x + rectWidth / 2)
                , Svg.Attributes.y <| toString (y + rectHeight / 2)
                , textAnchor "middle"
                , dominantBaseline "middle"
                , fontFamily "monospace"
                , fontSize <| toString textSize
                ]
                [ Svg.text text ]
            ]
