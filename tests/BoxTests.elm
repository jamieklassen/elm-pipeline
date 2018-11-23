module BoxTests exposing (..)

import Box
import Expect
import Helpers exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import Svg


all : Test
all =
    describe "box"
        [ describe "green box saying 'job' in white 12 pixel text" <|
            let
                greenRect =
                    Svg.svg []
                        [ Box.box
                            { backgroundColor = concourseGreen
                            , textColor = white
                            , text = "job"
                            , textSize = 12
                            , padding = 5
                            , x = 0
                            , y = 0
                            }
                        ]
                        |> Query.fromHtml
                        |> Query.find [ tag "g" ]
            in
                [ test "rect has 'job' written on it in white" <|
                    \_ ->
                        greenRect
                            |> Expect.all
                                [ Query.children [] >> Query.count (Expect.greaterThan 1)
                                , Query.children [ tag "text" ]
                                    >> Expect.all
                                        [ Query.count (Expect.equal 1)
                                        , Query.first >> Query.has [ text "job", isWhite ]
                                        ]
                                ]
                , test "rect text is in 12 pixel monospace font" <|
                    \_ ->
                        greenRect
                            |> Query.find [ tag "text" ]
                            |> Query.has
                                [ attribute "font-family" "monospace"
                                , attribute "font-size" "12"
                                ]
                , describe "rect size and text positioning" <|
                    let
                        glyphHeight =
                            12.0

                        -- we are naively assuming that in any monospaced font, each glyph is
                        -- 60% as wide as it is tall
                        glyphWidth =
                            12.0 * 0.6

                        padding =
                            5.0

                        textLength =
                            toFloat <| String.length "job"

                        rectWidth =
                            padding + textLength * glyphWidth + padding

                        rectHeight =
                            padding + glyphHeight + padding
                    in
                        [ test "rect is big enough to leave 5px padding on all sides of the text" <|
                            \_ ->
                                greenRect
                                    |> Query.find [ tag "rect" ]
                                    |> Query.has
                                        [ isConcourseGreen
                                        , attribute "width" <| toString rectWidth
                                        , attribute "height" <| toString rectHeight
                                        ]
                        , test "rect text is centered horizontally" <|
                            \_ ->
                                greenRect
                                    |> Query.find [ tag "text" ]
                                    |> Query.has
                                        [ attribute "text-anchor" "middle"
                                        , attribute "x" <| toString (rectWidth / 2)
                                        ]
                        , test "rect text is centered vertically" <|
                            \_ ->
                                greenRect
                                    |> Query.find [ tag "text" ]
                                    |> Query.has
                                        [ attribute "dominant-baseline" "middle"
                                        , attribute "y" <| toString (rectHeight / 2)
                                        ]
                        ]
                ]
        , describe "green box saying 'job' in white 24 pixel text offset 10 pixels to the right and down" <|
            let
                greenRect =
                    Svg.svg []
                        [ Box.box
                            { backgroundColor = concourseGreen
                            , textColor = white
                            , text = "job"
                            , textSize = 24
                            , padding = 5
                            , x = 10
                            , y = 10
                            }
                        ]
                        |> Query.fromHtml
                        |> Query.find [ tag "g" ]
            in
                [ test "rect has 'job' written on it in white" <|
                    \_ ->
                        greenRect
                            |> Expect.all
                                [ Query.children [] >> Query.count (Expect.greaterThan 1)
                                , Query.children [ tag "text" ]
                                    >> Expect.all
                                        [ Query.count (Expect.equal 1)
                                        , Query.first >> Query.has [ text "job", isWhite ]
                                        ]
                                ]
                , test "rect text is in 24 pixel monospace font" <|
                    \_ ->
                        greenRect
                            |> Query.find [ tag "text" ]
                            |> Query.has
                                [ attribute "font-family" "monospace"
                                , attribute "font-size" "24"
                                ]
                , describe "rect size and text positioning" <|
                    let
                        glyphHeight =
                            24.0

                        glyphWidth =
                            24.0 * 0.6

                        padding =
                            5.0

                        textLength =
                            toFloat <| String.length "job"

                        rectWidth =
                            padding + textLength * glyphWidth + padding

                        rectHeight =
                            padding + glyphHeight + padding
                    in
                        [ test "rect is big enough to leave 5px padding on all sides of the text" <|
                            \_ ->
                                greenRect
                                    |> Query.find [ tag "rect" ]
                                    |> Query.has
                                        [ isConcourseGreen
                                        , attribute "width" <| toString rectWidth
                                        , attribute "height" <| toString rectHeight
                                        ]
                        , test "rect text is centered horizontally" <|
                            \_ ->
                                greenRect
                                    |> Query.find [ tag "text" ]
                                    |> Query.has
                                        [ attribute "text-anchor" "middle"
                                        , attribute "x" <| toString (10 + rectWidth / 2)
                                        ]
                        , test "rect text is centered vertically" <|
                            \_ ->
                                greenRect
                                    |> Query.find [ tag "text" ]
                                    |> Query.has
                                        [ attribute "dominant-baseline" "middle"
                                        , attribute "y" <| toString (10 + rectHeight / 2)
                                        ]
                        ]
                ]
        ]
