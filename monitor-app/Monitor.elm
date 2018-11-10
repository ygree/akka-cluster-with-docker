module Main exposing (Model, Msg(..), getClusterMembers, getClusterMembersReq, linkElement, main, model, nodeElement, renderGraph, sourceUrls, subscriptions, update, view, viewNodes)

--import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)

import AkkaCluster.Graph as Graph exposing (GraphEntity, GraphNodes)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, NodeAddress, decodeMembers)
import AkkaCluster.Nodes as Nodes exposing (NodeUrl, Nodes, nodeInfo)
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Browser
import Browser.Events exposing (onAnimationFrame)
import Dict
import Force as Force exposing (State)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as Decode exposing (Value)
import List exposing (..)
import Maybe exposing (withDefault)
import Set exposing (Set)
import String exposing (fromFloat)
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Posix, every, millisToPosix)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { nodes : Nodes
    , graph : Graph.GraphNodes
    }


model : Model
model =
    { nodes = Nodes.empty
    , graph = Graph.emptyGraphNodes
    }


subscriptions : Model -> Sub Msg
subscriptions model1 =
    if Force.isCompleted model1.graph.simulation then
        Sub.none

    else
        --        Sub.none
        onAnimationFrame Tick



-- UPDATE


type Msg
    = Fetch
    | ClusterMembersResp NodeUrl (Result Http.Error ClusterMembers)
    | Tick Posix


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model1 =
    case msg of
        Fetch ->
            ( model1, Cmd.batch <| List.map getClusterMembers sourceUrls )

        ClusterMembersResp nodeUrl (Ok result) ->
            let
                newNodes =
                    Nodes.insertClusterMembers model1.nodes nodeUrl result

                newGraph =
                    Graph.updateGraphNodes model1.graph newNodes
            in
            ( { model1 | nodes = newNodes, graph = newGraph }, Cmd.none )

        ClusterMembersResp nodeUrl (Err err) ->
            ( { model1 | nodes = Nodes.removeClusterMembers model1.nodes nodeUrl }, Cmd.none )

        Tick t ->
            let
                ( newSimulation, newEntities ) =
                    Force.tick model1.graph.simulation model1.graph.entities

                graph =
                    model1.graph
            in
            ( { model1 | graph = { graph | entities = newEntities, simulation = newSimulation } }, Cmd.none )


sourceUrls : List NodeUrl
sourceUrls =
    List.map (\n -> "http://localhost:8558/node-" ++ fromFloat n ++ "/cluster/members") [ 1, 2, 3, 4, 5 ]


getClusterMembers : NodeUrl -> Cmd Msg
getClusterMembers nodeUrl =
    Http.send (ClusterMembersResp nodeUrl)
        (getClusterMembersReq nodeUrl decodeMembers)


getClusterMembersReq : String -> Decode.Decoder a -> Request a
getClusterMembersReq url decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = emptyBody
        , expect = expectJson decoder
        , timeout = Just <| 2 * 1000 -- millis
        , withCredentials = False
        }



-- VIEW


view : Model -> Html Msg
view model1 =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ Button.button [ Button.primary, Button.onClick Fetch ] [ text "Fetch" ]

                -- , div [] [ text (toString model.nodes) ]
                , viewNodes model1.nodes
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ renderGraph model1.nodes model1.graph ]
            ]
        ]


viewNodes : Nodes -> Html Msg
viewNodes nodes =
    let
        drawNodeCell : NodeUrl -> NodeAddress -> Table.Cell Msg
        drawNodeCell source node =
            case nodeInfo nodes source node of
                Nothing ->
                    Table.td [] [ text "" ]

                Just { status, isLeader, isOldest, isUnreachable } ->
                    let
                        leaderLabel =
                            if isLeader then
                                [ "leader" ]

                            else
                                []

                        oldestLabel =
                            if isOldest then
                                [ "oldest" ]

                            else
                                []

                        labels =
                            foldr (++) "" <| intersperse " | " <| leaderLabel ++ oldestLabel

                        unreachablePrefix =
                            if isUnreachable then
                                "(?) "

                            else
                                ""

                        statusLabel =
                            case status of
                                Nodes.UnknownNodeStatus ->
                                    "?"

                                Nodes.NodeStatus memberStatus ->
                                    unreachablePrefix ++ Debug.toString memberStatus
                    in
                    Table.td []
                        [ div [] [ text statusLabel ]
                        , div [] [ text labels ]
                        ]

        --    sortedAllNodes = Nodes.allNodeInfo nodes |> Dict.keys |> List.sort
        sortedAllNodes =
            Nodes.allNodes nodes |> Set.toList |> List.sort

        nodeTableHeaders : List (Html Msg)
        nodeTableHeaders =
            List.map (\nodeId -> text <| Nodes.nodeHostname nodeId) <| sortedAllNodes

        drawNodeSource source =
            Table.td [ Table.cellAttr <| title source ] [ text <| Nodes.sourceHostname nodes source ]

        drawNodeRow : NodeUrl -> Table.Row Msg
        drawNodeRow source =
            Table.tr [] <| drawNodeSource source :: List.map (drawNodeCell source) sortedAllNodes
    in
    Table.table
        { options = [ Table.striped, Table.hover, Table.small, Table.bordered ]
        , thead = Table.simpleThead <| List.map (\v -> Table.th [] [ v ]) (text "source" :: nodeTableHeaders)
        , tbody = Table.tbody [] (List.map drawNodeRow <| Nodes.sourceNodes nodes)
        }



---


renderGraph : Nodes -> Graph.GraphNodes -> Svg Msg
renderGraph nodes graphNodes =
    svg [ width (fromFloat Graph.screenWidth ++ "px"), height (fromFloat Graph.screenHeight ++ "px") ]
        [ Svg.g [ class "links" ] <| List.map (linkElement nodes graphNodes []) <| Set.toList graphNodes.links
        , Svg.g [ class "links" ] <| List.map (linkElement nodes graphNodes [ strokeDasharray "2, 10" ]) <| Set.toList graphNodes.unreachableLinks
        , Svg.g [ class "nodes" ] <| List.map (nodeElement nodes) <| graphNodes.entities
        ]


linkElement : Nodes -> Graph.GraphNodes -> List (Attribute a) -> Graph.NodesLink -> Svg a
linkElement nodes graph attrs edge =
    let
        source =
            List.head <| List.filter (\e -> e.id == Tuple.first edge) graph.entities

        target =
            List.head <| List.filter (\e -> e.id == Tuple.second edge) graph.entities
    in
    withDefault (div [] []) <|
        Maybe.map2
            (\s t ->
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
            )
            source
            target


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
