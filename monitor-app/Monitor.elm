import AkkaCluster.Graph exposing (GraphNodes)
import Dict
import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button

import Html.Events exposing (onClick)
import Http exposing (..)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, NodeAddress, decodeMembers)
import List exposing (..)
import Maybe exposing (withDefault)
import Set exposing (Set)
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)
import AnimationFrame
--import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Svg
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)
import AkkaCluster.Nodes as Nodes exposing (NodeUrl, Nodes, nodeInfo)
import AkkaCluster.Graph as Graph
import Json.Decode as Decode

main =
  Html.program { init = (model, Cmd.none)
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
model = { nodes = Nodes.empty
        , graph = Graph.emptyGraphNodes
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    if Force.isCompleted model.graph.simulation then
        Sub.none
    else
--        Sub.none
        AnimationFrame.times Tick

-- UPDATE

type Msg = Fetch
         | ClusterMembersResp NodeUrl (Result Http.Error ClusterMembers)
         | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch -> 
      (model, Cmd.batch <| List.map getClusterMembers sourceUrls)

    ClusterMembersResp nodeUrl (Ok result) ->
      let
        newNodes = Nodes.insertClusterMembers model.nodes nodeUrl result
        newGraph = Graph.updateGraphNodes model.graph newNodes
      in
        ({ model | nodes = newNodes, graph = newGraph }, Cmd.none)

    ClusterMembersResp nodeUrl (Err err) ->
      ({ model | nodes = Nodes.removeClusterMembers model.nodes nodeUrl }, Cmd.none)

    Tick t -> let
                (newSimulation, newEntities) = Force.tick model.graph.simulation model.graph.entities
                graph = model.graph
              in
                ({ model | graph = { graph | entities = newEntities, simulation = newSimulation }}, Cmd.none)

sourceUrls : List NodeUrl
sourceUrls = List.map (\n -> "http://localhost:8558/node-" ++ toString n ++ "/cluster/members") [1,2,3,4,5]

getClusterMembers : NodeUrl -> Cmd Msg
getClusterMembers nodeUrl = Http.send (ClusterMembersResp nodeUrl)
                                      (getClusterMembersReq nodeUrl decodeMembers)

getClusterMembersReq : String -> Decode.Decoder a -> Request a
getClusterMembersReq url decoder =
  Http.request
    { method = "GET"
    , headers = []
    , url = url
    , body = emptyBody
    , expect = expectJson decoder
    , timeout = Just <| 2 * second
    , withCredentials = False
    }

-- VIEW

view : Model -> Html Msg
view model =
  Grid.container []
    [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
    , Grid.row []
        [ Grid.col []
          [ Button.button [ Button.primary, Button.onClick Fetch ] [ text "Fetch" ]
          -- , div [] [ text (toString model.nodes) ]
          , viewNodes model.nodes
          ]
        ]
    , Grid.row []
        [ Grid.col []
          [ renderGraph model.nodes model.graph ]
        ]
    ]

viewNodes : Nodes -> Html Msg
viewNodes nodes =
  let
    drawNodeCell : NodeUrl -> NodeAddress -> Table.Cell Msg
    drawNodeCell source node =
        case nodeInfo nodes source node of
          Nothing -> Table.td [] [ text "N/A" ]
          Just { status, isLeader, isOldest } ->
            let
              leaderLabel = if isLeader then ["leader"] else []
              oldestLabel = if isOldest then ["oldest"] else []
              labels = foldr (++) "" <| intersperse " | " <| leaderLabel ++ oldestLabel
              statusLabel = case status of
                              Nodes.UnknownNodeStatus -> "???"
                              Nodes.NodeStatus memberStatus -> toString memberStatus
            in Table.td []
                    [ div [] [ text statusLabel ]
                    , div [] [ text labels ]
                    ]

    sortedAllNodes = Nodes.allNodeInfo nodes |> Dict.keys |> List.sort

    nodeTableHeaders : List (Html Msg)
    nodeTableHeaders = List.map (\nodeId -> text <| Nodes.nodeHostname nodeId) <| sortedAllNodes

    drawNodeSource source = Table.td [ Table.cellAttr <| title source ] [ text <| Nodes.sourceHostname nodes source ]

    drawNodeRow : NodeUrl -> Table.Row Msg
    drawNodeRow source = Table.tr [] <| (drawNodeSource source) :: List.map (drawNodeCell source) sortedAllNodes
  in
  Table.table
    { options = [ Table.striped, Table.hover, Table.small, Table.bordered ]
    , thead = Table.simpleThead <| List.map (\v -> Table.th [] [v]) (text "source" :: nodeTableHeaders)
    , tbody = Table.tbody [] (List.map drawNodeRow <| Nodes.sourceNodes nodes)
    }

---


renderGraph : Nodes -> Graph.GraphNodes -> Svg Msg
renderGraph nodes graphNodes =
    svg [ width (toString Graph.screenWidth ++ "px"), height (toString Graph.screenHeight ++ "px") ]
        [ Svg.g [ class "links" ] <| List.map (linkElement nodes graphNodes) <| Set.toList graphNodes.links
        , Svg.g [ class "nodes" ] <| List.map (nodeElement nodes) <| graphNodes.entities
        ]


linkElement : Nodes -> Graph.GraphNodes -> Graph.NodesLink -> Svg a
linkElement nodes graph edge =
    let
        source = List.head <| List.filter (\e -> e.id == Tuple.first edge) graph.entities
        target = List.head <| List.filter (\e -> e.id == Tuple.second edge) graph.entities
    in withDefault (div [] []) <| Maybe.map2 (\s t ->
        line
            [ strokeWidth "1"
            , stroke "#aaa"
            , x1 (toString s.x)
            , y1 (toString s.y)
            , x2 (toString t.x)
            , y2 (toString t.y)
            ]
            []
            ) source target


nodeElement : Nodes -> Graph.Entity -> Svg a
nodeElement nodes node =
  let
    color = if node.value.isLeader
            then "#ff99ff"
            else "#99ccff"

    strokeColor = if node.value.isOldest
                  then "#ffcc00"
                  else color
  in
    svg []
      [
      circle
        [ r "8.5"
        , fill color
        , stroke strokeColor --"transparent"
        , strokeWidth "3px"
--        , onMouseDown node.id
        , cx (toString node.x)
        , cy (toString node.y)
        ]
        [ Svg.title [] [ Svg.text <| Nodes.nodeHostname node.id ] ]
      ]