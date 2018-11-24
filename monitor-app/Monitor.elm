module Main exposing (main)

import AkkaCluster.Graph as Graph exposing (GraphEntity, GraphNodes)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, NodeAddress, decodeMembers, toString)
import AkkaCluster.Nodes as Nodes exposing (NodeUrl, Nodes, nodeInfo)
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Browser
import Browser.Events exposing (onAnimationFrame)
import Dict
import DrawGraph exposing (renderGraph)
import Force as Force exposing (State)
import Html exposing (..)
import Html.Attributes exposing (checked, title, type_)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as Decode exposing (Value)
import List exposing (..)
import Set exposing (Set)
import String exposing (fromFloat)
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
    , autoFetch : Bool
    }


initModel : Model
initModel =
    { nodes = Nodes.empty
    , graph = Graph.emptyGraphNodes
    , autoFetch = True
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        fetchSub =
            if model.autoFetch then
                Time.every 1000 (\_ -> Fetch)

            else
                Sub.none

        tickSub =
            if Force.isCompleted model.graph.simulation then
                Sub.none

            else
                onAnimationFrame Tick
    in
    Sub.batch [ fetchSub, tickSub ]



-- UPDATE


type Msg
    = Fetch
    | ClusterMembersResp NodeUrl (Result Http.Error ClusterMembers)
    | Tick Posix
    | ToggleAutoFetch



-- The first param is used for passing some values from JavaScript but in fact
-- it's unused here and left only because it's mandatory.
-- It supports several types, I've peeked one that can handle arbitrary Json.
-- TODO: How to get rid of this param?


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, fetchData )

        ClusterMembersResp nodeUrl (Ok result) ->
            let
                newNodes =
                    Nodes.insertClusterMembers model.nodes nodeUrl result

                newGraph =
                    Graph.updateGraphNodes model.graph newNodes
            in
            if model.nodes == newNodes then
                -- do not refresh the model if the nodes haven't changed
                ( model, Cmd.none )

            else
                ( { model | nodes = newNodes, graph = newGraph }, Cmd.none )

        ClusterMembersResp nodeUrl (Err err) ->
            ( { model | nodes = Nodes.removeClusterMembers model.nodes nodeUrl }, Cmd.none )

        Tick t ->
            let
                ( newSimulation, newEntities ) =
                    Force.tick model.graph.simulation model.graph.entities

                graph =
                    model.graph
            in
            ( { model | graph = { graph | entities = newEntities, simulation = newSimulation } }, Cmd.none )

        ToggleAutoFetch ->
            ( { model | autoFetch = not model.autoFetch }, Cmd.none )


fetchData : Cmd Msg
fetchData =
    let
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
    in
    Cmd.batch <| List.map getClusterMembers sourceUrls



-- VIEW


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name isChecked =
    let
        attrs =
            [ type_ "checkbox", onClick msg, checked isChecked ]
    in
    label []
        [ input attrs []
        , text name
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS

        -- , div [] [ text (Debug.toString model.autoFetch) ]
        , Grid.row []
            [ Grid.col []
                [ Button.button [ Button.primary, Button.onClick Fetch ] [ text "Fetch" ]
                , checkbox ToggleAutoFetch "Auto-fetch" model.autoFetch
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
                                    unreachablePrefix ++ toString memberStatus
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
