module GraphTests exposing (..)

import Expect
import Graph
import Test exposing (..)


suite : Test
suite =
    describe "laying a graph into columns" <|
        [ test "an empty graph has no columns" <|
            \_ ->
                []
                    |> Graph.columns
                    |> Expect.equal []
        , test "a singleton graph has one column" <|
            let
                node =
                    { id = 0, text = "", color = "", neighbours = [] }
            in
                \_ ->
                    [ node ]
                        |> Graph.columns
                        |> Expect.equal [ [ node ] ]
        , test "a circular singleton graph has one column" <|
            let
                node =
                    { id = 0, text = "", color = "", neighbours = [ 0 ] }
            in
                \_ ->
                    [ node ]
                        |> Graph.columns
                        |> Expect.equal [ [ node ] ]
        , test "a two-node loop has two columns" <|
            let
                node1 =
                    { id = 0, text = "", color = "", neighbours = [ 1 ] }

                node2 =
                    { id = 1, text = "", color = "", neighbours = [ 0 ] }
            in
                \_ ->
                    [ node1, node2 ]
                        |> Graph.columns
                        |> Expect.equal [ [ node1 ], [ node2 ] ]
        , test "parent and child end up in separate columns" <|
            let
                parent =
                    { id = 0, text = "", color = "", neighbours = [ 1 ] }

                child =
                    { id = 1, text = "", color = "", neighbours = [] }
            in
                \_ ->
                    [ parent, child ]
                        |> Graph.columns
                        |> Expect.equal [ [ parent ], [ child ] ]
        , test "multiple nodes with no children are in one column" <|
            let
                node1 =
                    { id = 0, text = "", color = "", neighbours = [] }

                node2 =
                    { id = 1, text = "", color = "", neighbours = [] }
            in
                \_ ->
                    [ node1, node2 ]
                        |> Graph.columns
                        |> Expect.equal [ [ node1, node2 ] ]
        , test "parent, child and grandchild end up in separate columns" <|
            let
                parent =
                    { id = 0, text = "", color = "", neighbours = [ 1 ] }

                child =
                    { id = 1, text = "", color = "", neighbours = [ 2 ] }

                grandchild =
                    { id = 2, text = "", color = "", neighbours = [] }
            in
                \_ ->
                    [ parent, child, grandchild ]
                        |> Graph.columns
                        |> Expect.equal [ [ parent ], [ child ], [ grandchild ] ]
        ]
