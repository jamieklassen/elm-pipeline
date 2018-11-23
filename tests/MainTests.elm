module MainTests exposing (..)

import Box
import Expect
import Main
import Helpers exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (containing, Selector, tag, text, style)


greenRectGroup : Selector
greenRectGroup =
    Selector.all [ tag "g", containing [ tag "rect", isConcourseGreen ] ]


horizontalSpaceBetweenBoxes : Float
horizontalSpaceBetweenBoxes =
    200


verticalSpaceBetweenBoxes : Float
verticalSpaceBetweenBoxes =
    25


bezierControlPointDistance : Float
bezierControlPointDistance =
    horizontalSpaceBetweenBoxes / 2


darkGrey : String
darkGrey =
    "#3d3c3c"


suite : Test
suite =
    describe "app" <|
        let
            init =
                Main.init
                    |> Tuple.first
                    |> Main.view
                    |> Query.fromHtml

            greenBoxWidth =
                Box.width
                    { text = "job-name"
                    , textSize = 36
                    , padding = 10
                    }

            boxHeight =
                10 * 2 + 36
        in
            [ describe "overall page styles"
                [ test "page has dark grey background" <|
                    \_ -> init |> Query.has [ style [ ( "background-color", darkGrey ) ] ]
                , test "background takes entire height of page" <|
                    \_ -> init |> Query.has [ style [ ( "height", "100%" ) ] ]
                , test "background has healthy padding" <|
                    \_ -> init |> Query.has [ style [ ( "padding", "20px" ) ] ]
                ]
            , describe "svg" <|
                let
                    svg =
                        init |> Query.find [ tag "svg" ]
                in
                    [ test "has an svg" <|
                        \_ ->
                            init
                                |> Query.has [ tag "svg" ]
                    , test "svg is as wide as the whole viewport" <|
                        \_ ->
                            svg
                                |> Expect.all
                                    [ Query.has [ attribute "width" "100%" ]
                                    ]
                    , test "svg is as tall as the whole viewport" <|
                        \_ ->
                            svg
                                |> Expect.all
                                    [ Query.has [ attribute "height" "100%" ]
                                    ]
                    , describe "'job-name' green box" <|
                        let
                            greenBox =
                                svg
                                    |> Query.find [ greenRectGroup, containing [ text "job-name" ] ]
                        in
                            [ test "has padding of 10 pixels around text" <|
                                \_ ->
                                    greenBox
                                        |> Query.find [ tag "rect" ]
                                        |> Query.has
                                            [ attribute "width" <| toString greenBoxWidth
                                            ]
                            ]
                    , describe "'other-job' green box" <|
                        let
                            greenBox =
                                svg
                                    |> Query.find [ greenRectGroup, containing [ text "other-job" ] ]
                        in
                            [ test "exists" <|
                                \_ ->
                                    svg |> Query.has [ greenRectGroup, containing [ text "other-job" ] ]
                            , test "has same x position as red box" <|
                                \_ ->
                                    greenBox
                                        |> Query.find [ tag "rect" ]
                                        |> Query.has [ attribute "x" <| toString (greenBoxWidth + horizontalSpaceBetweenBoxes) ]
                            , test "sits 50px below red box" <|
                                \_ ->
                                    greenBox
                                        |> Query.find [ tag "rect" ]
                                        |> Query.has [ attribute "y" <| toString (boxHeight + verticalSpaceBetweenBoxes) ]
                            ]
                    , describe "red box" <|
                        [ test "svg has a red box" <|
                            \_ ->
                                svg
                                    |> Query.has
                                        [ Selector.all [ tag "g", containing [ tag "rect", isConcourseRed ] ] ]
                        , test "red box is positioned 100px to the right of the green box" <|
                            \_ ->
                                svg
                                    |> Query.find
                                        [ Selector.all [ tag "g", containing [ tag "rect", isConcourseRed ] ]
                                        ]
                                    |> Query.find [ tag "rect" ]
                                    |> Query.has [ attribute "x" <| toString (greenBoxWidth + horizontalSpaceBetweenBoxes) ]
                        ]
                    , describe "line connecting green and red boxes" <|
                        let
                            curveY =
                                toString (boxHeight / 2)

                            startPointX =
                                toString greenBoxWidth

                            firstControlPointX =
                                toString <| greenBoxWidth + bezierControlPointDistance

                            secondControlPointX =
                                toString <| greenBoxWidth + horizontalSpaceBetweenBoxes - bezierControlPointDistance

                            endpointX =
                                toString <| greenBoxWidth + horizontalSpaceBetweenBoxes

                            connectingLineAttr =
                                attribute "d" <|
                                    "M"
                                        ++ startPointX
                                        ++ " "
                                        ++ curveY
                                        ++ " C "
                                        ++ firstControlPointX
                                        ++ " "
                                        ++ curveY
                                        ++ ", "
                                        ++ secondControlPointX
                                        ++ " "
                                        ++ curveY
                                        ++ ", "
                                        ++ endpointX
                                        ++ " "
                                        ++ curveY

                            connectingLine =
                                svg |> Query.find [ tag "path", connectingLineAttr ]
                        in
                            [ test "there is a line connecting the green and red boxes" <|
                                \_ ->
                                    svg
                                        |> Query.has [ tag "path", connectingLineAttr ]
                            , test "connecting line is light grey" <|
                                \_ ->
                                    connectingLine |> Query.has [ attribute "stroke" "#7a7373" ]
                            , test "connecting line is 2 pixels wide" <|
                                \_ ->
                                    connectingLine |> Query.has [ attribute "stroke-width" "2" ]
                            ]
                    , describe
                        ("line connecting the right midpoint of the green box"
                            ++ "to the left midpoint of the other green box"
                        )
                      <|
                        let
                            startingY =
                                toString (boxHeight / 2)

                            endingY =
                                toString (boxHeight + verticalSpaceBetweenBoxes + boxHeight / 2)

                            startPointX =
                                toString greenBoxWidth

                            firstControlPointX =
                                toString <| greenBoxWidth + bezierControlPointDistance

                            secondControlPointX =
                                toString <| greenBoxWidth + horizontalSpaceBetweenBoxes - bezierControlPointDistance

                            endpointX =
                                toString <| greenBoxWidth + horizontalSpaceBetweenBoxes

                            connectingLineAttr =
                                attribute "d" <|
                                    "M"
                                        ++ startPointX
                                        ++ " "
                                        ++ startingY
                                        ++ " C "
                                        ++ firstControlPointX
                                        ++ " "
                                        ++ startingY
                                        ++ ", "
                                        ++ secondControlPointX
                                        ++ " "
                                        ++ endingY
                                        ++ ", "
                                        ++ endpointX
                                        ++ " "
                                        ++ endingY
                        in
                            [ test "exists" <|
                                \_ ->
                                    svg |> Query.has [ connectingLineAttr ]
                            , test "has transparent fill color" <|
                                \_ ->
                                    svg
                                        |> Query.find [ connectingLineAttr ]
                                        |> Query.has [ attribute "fill" "transparent" ]
                            ]
                    ]
            ]
