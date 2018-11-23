module Graph exposing (columns, Graph)

import List.Extra


type alias Node =
    { id : Int
    , text : String
    , color : String
    , neighbours : List Int
    }


type alias Graph =
    List Node


type alias Column =
    List Node


type alias Path =
    List Node


columns : Graph -> List Column
columns graph =
    let
        isChild : Node -> Node -> Bool
        isChild child parent =
            List.member child.id parent.neighbours

        depthFirstSearch : Graph -> List Path -> Path -> Node -> List Node -> List Path
        depthFirstSearch graph accPaths currentPath initial visited =
            let
                currentNode =
                    currentPath
                        |> List.Extra.last
                        |> Maybe.withDefault initial

                candidates =
                    graph
                        |> List.filter (isChild currentNode)
                        |> List.filter (not << flip List.member visited)
            in
                accPaths
                    ++ List.concatMap
                        (\n ->
                            depthFirstSearch
                                graph
                                (accPaths ++ [ currentPath ])
                                (currentPath ++ [ n ])
                                initial
                                (visited ++ [ n ])
                        )
                        candidates

        generation : List Node -> Node -> Int
        generation nodes node =
            let
                others =
                    nodes |> List.Extra.remove node

                parents =
                    others |> List.filter (isChild node)

                maxParentGeneration =
                    parents
                        |> List.map (generation others)
                        |> List.maximum
                        |> Maybe.withDefault -1
            in
                maxParentGeneration + 1

        groupByAndSort : (a -> comparable) -> List a -> List (List a)
        groupByAndSort mapper list =
            list
                |> List.map mapper
                |> List.Extra.unique
                |> List.sort
                |> List.map
                    (\mapperValue -> list |> List.filter (mapper >> (==) mapperValue))
    in
        groupByAndSort
            (\n ->
                depthFirstSearch graph [] [] n []
                    |> List.map List.length
                    |> List.maximum
                    |> Maybe.withDefault 0
            )
            graph
