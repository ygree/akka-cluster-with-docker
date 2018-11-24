module DrawGraph exposing (renderGraph)

import AkkaCluster.Graph as Graph exposing (GraphEntity, GraphNodes)
import AkkaCluster.Nodes as Nodes exposing (NodeUrl, Nodes, nodeInfo)
import Maybe exposing (withDefault)
import Set exposing (Set)
import String exposing (fromFloat)
import Svg exposing (Attribute, Svg, circle, line, rect, svg)
import Svg.Attributes exposing (..)


renderGraph : Nodes -> Graph.GraphNodes -> Svg m
renderGraph nodes graphNodes =
    svg [ width (fromFloat Graph.screenWidth ++ "px"), height (fromFloat Graph.screenHeight ++ "px") ]
        [ Svg.g [ class "links" ] <| List.map (linkElement nodes graphNodes []) <| Set.toList graphNodes.links
        , Svg.g [ class "links" ] <| List.map (linkElement nodes graphNodes [ strokeDasharray "2, 10" ]) <| Set.toList graphNodes.unreachableLinks
        , Svg.g [ class "nodes" ] <| List.map (nodeElement nodes) <| graphNodes.entities
        ]


linkElement : Nodes -> Graph.GraphNodes -> List (Attribute a) -> Graph.NodesLink -> Svg a
linkElement nodes graph attrs edge =
    let
        source : Maybe GraphEntity
        source =
            List.head <| List.filter (\e -> e.id == Tuple.first edge) graph.entities

        target : Maybe GraphEntity
        target =
            List.head <| List.filter (\e -> e.id == Tuple.second edge) graph.entities

        drawLine : GraphEntity -> GraphEntity -> Svg a
        drawLine s t =
            line
                (List.append
                    [ strokeWidth "1"
                    , stroke "#aaa"
                    , x1 (fromFloat s.x)
                    , y1 (fromFloat s.y)
                    , x2 (fromFloat t.x)
                    , y2 (fromFloat t.y)
                    ]
                    attrs
                )
                []
    in
    withDefault (Svg.text "") (Maybe.map2 drawLine source target)


nodeElement : Nodes -> GraphEntity -> Svg a
nodeElement nodes node =
    let
        color =
            if node.value.isUnreachable then
                "#cecece"

            else if node.value.isLeader then
                "#ff99ff"
                --TODO mark leaving and exiting node
                --            else if node.value.status

            else
                "#99ccff"

        strokeColor =
            if node.value.isOldest then
                "#ff2b2b"

            else
                color
    in
    svg []
        [ circle
            [ r "8.5"
            , fill color
            , stroke strokeColor --"transparent"
            , strokeWidth "3px"

            --        , onMouseDown node.id
            , cx (fromFloat node.x)
            , cy (fromFloat node.y)
            ]
            [ Svg.title [] [ Svg.text <| Nodes.nodeHostname node.id ] ]
        ]
