module Main exposing (..)

import Box
import Graph exposing (Graph)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, width)


-- TYPES


type alias Model =
    Graph


type Msg
    = Msg



-- CONSTANTS


concourseGreen : String
concourseGreen =
    "#11c560"


concourseRed : String
concourseRed =
    "#ed4b35"


darkGrey : String
darkGrey =
    "#3d3c3c"


white : String
white =
    "#fff"


rectSize : Int
rectSize =
    20



-- FUNCTIONS


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( [ { id = 0
        , text = "job-name"
        , color = concourseGreen
        , neighbours = [ 1, 2 ]
        }
      , { id = 1
        , text = "failing-job"
        , color = concourseRed
        , neighbours = []
        }
      , { id = 2
        , text = "other-job"
        , color = concourseGreen
        , neighbours = []
        }
      ]
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


horizontalSpaceBetweenBoxes : Float
horizontalSpaceBetweenBoxes =
    200


verticalSpaceBetweenBoxes : Float
verticalSpaceBetweenBoxes =
    25


view : Model -> Html Msg
view model =
    let
        textSize =
            36

        padding =
            10
    in
        Html.div
            [ style
                [ ( "background-color", darkGrey )
                , ( "height", "100%" )
                , ( "padding", "20px" )
                ]
            ]
            [ Svg.svg
                [ width "100%"
                , height "100%"
                ]
                [ Box.box
                    { backgroundColor = concourseGreen
                    , textColor = white
                    , text = "job-name"
                    , textSize = textSize
                    , padding = padding
                    , x = 0
                    , y = 0
                    }
                , line
                    { startingX = Box.width { text = "job-name", textSize = textSize, padding = padding }
                    , startingY = Box.height { textSize = textSize, padding = padding } / 2
                    , endingX = Box.width { text = "job-name", textSize = textSize, padding = padding } + horizontalSpaceBetweenBoxes
                    , endingY = Box.height { textSize = textSize, padding = padding } / 2
                    }
                , Box.box
                    { backgroundColor = concourseRed
                    , textColor = white
                    , text = "failing-job"
                    , textSize = textSize
                    , padding = padding
                    , x = Box.width { text = "job-name", textSize = textSize, padding = padding } + horizontalSpaceBetweenBoxes
                    , y = 0
                    }
                , line
                    { startingX = Box.width { text = "job-name", textSize = textSize, padding = padding }
                    , startingY = Box.height { textSize = textSize, padding = padding } / 2
                    , endingX = Box.width { text = "job-name", textSize = textSize, padding = padding } + horizontalSpaceBetweenBoxes
                    , endingY =
                        Box.height { textSize = textSize, padding = padding }
                            + verticalSpaceBetweenBoxes
                            + (Box.height { textSize = textSize, padding = padding } / 2)
                    }
                , Box.box
                    { backgroundColor = concourseGreen
                    , textColor = white
                    , text = "other-job"
                    , textSize = textSize
                    , padding = padding
                    , x = Box.width { text = "job-name", textSize = textSize, padding = padding } + horizontalSpaceBetweenBoxes
                    , y = Box.height { textSize = textSize, padding = padding } + verticalSpaceBetweenBoxes
                    }
                ]
            ]


bezierControlPointDistance : Float
bezierControlPointDistance =
    horizontalSpaceBetweenBoxes / 2


line : { startingX : Float, startingY : Float, endingX : Float, endingY : Float } -> Svg.Svg msg
line { startingX, startingY, endingX, endingY } =
    let
        startPointX =
            toString startingX

        startPointY =
            toString startingY

        firstControlPointX =
            toString <| startingX + bezierControlPointDistance

        firstControlPointY =
            toString <| startingY

        secondControlPointX =
            toString <| endingX - bezierControlPointDistance

        secondControlPointY =
            toString <| endingY

        endPointX =
            toString endingX

        endPointY =
            toString endingY
    in
        Svg.path
            [ d <|
                "M"
                    ++ startPointX
                    ++ " "
                    ++ startPointY
                    ++ " C "
                    ++ firstControlPointX
                    ++ " "
                    ++ firstControlPointY
                    ++ ", "
                    ++ secondControlPointX
                    ++ " "
                    ++ secondControlPointY
                    ++ ", "
                    ++ endPointX
                    ++ " "
                    ++ endPointY
            , fill "transparent"
            , strokeWidth "2"
            , stroke "#7a7373"
            ]
            []
