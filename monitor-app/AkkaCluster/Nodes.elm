module AkkaCluster.Nodes exposing
    ( NodeInfo
    , NodeStatus(..)
    , NodeUrl
    , Nodes
    , allLinks
    , allNodeInfo
    , allNodes
    , empty
    , insertClusterMembers
    , leaderLinks
    , leaders
    , nodeHostname
    , nodeInfo
    , removeClusterMembers
    , sourceHostname
    , sourceNodes
    , unreachableLinks
    )

import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, MemberStatus, NodeAddress, decodeMembers)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Regex exposing (..)
import Set exposing (Set)


type alias Nodes =
    Dict NodeUrl ClusterNode


type alias ClusterNode =
    { selfNode : NodeAddress
    , knownNodes : Dict NodeAddress NodeInfo
    }


empty : Nodes
empty =
    Dict.empty


type alias NodeUrl =
    String


type NodeStatus
    = NodeStatus AkkaCluster.Json.MemberStatus
    | UnknownNodeStatus


type alias NodeInfo =
    { status : NodeStatus
    , isLeader : Bool
    , isOldest : Bool
    , isUnreachable : Bool
    }


insertClusterMembers : Nodes -> NodeUrl -> ClusterMembers -> Nodes
insertClusterMembers nodes nodeUrl clusterMembers =
    Dict.insert nodeUrl (clusterMembersToClusterNode clusterMembers) nodes


removeClusterMembers : Nodes -> NodeUrl -> Nodes
removeClusterMembers nodes nodeUrl =
    Dict.remove nodeUrl nodes


nodeHostname : NodeAddress -> String
nodeHostname node = node
    --TODO: Regex.fromString now returns Maybe
    -- withDefault node <| List.head <| List.drop 2 <| splitAtMost 3 (Regex.fromString "[@:]") node 

-- abc : NodeAddress -> Maybe String
-- abc node = Maybe.map (\r -> splitAtMost 3 r node) (Regex.fromString "[@:]")

sourceNodes : Nodes -> List NodeUrl
sourceNodes nodes =
    List.sortBy (sourceHostname nodes) (Dict.keys nodes)


allNodes : Nodes -> Set NodeAddress
allNodes nodes =
    Dict.values nodes
        |> List.concatMap (\v -> Dict.keys v.knownNodes)
        |> Set.fromList


allNodeInfo : Nodes -> Dict NodeAddress NodeInfo
allNodeInfo nodes =
    Dict.values nodes
        |> List.filterMap
            (\v ->
                Maybe.map (\info -> ( v.selfNode, info ))
                    (Dict.get v.selfNode v.knownNodes)
            )
        |> Dict.fromList



--maybeMemberStatus : NodeAddress -> ClusterMembers -> Maybe String
--maybeMemberStatus node cm = List.head <| List.map (\m -> toString m.status)
--                                      <| List.filter (\m -> m.node == node) cm.members


sourceHostname : Nodes -> NodeUrl -> String
sourceHostname nodes source =
    withDefault source <| Maybe.map nodeHostname (sourceNode nodes source)


nodeInfo : Nodes -> NodeUrl -> NodeAddress -> Maybe NodeInfo
nodeInfo nodes source node =
    nodes
        |> Dict.get source
        |> Maybe.map .knownNodes
        |> Maybe.andThen (Dict.get node)


allLinks : Nodes -> List ( NodeAddress, NodeAddress )
allLinks nodes =
    nodes
        |> Dict.values
        |> List.concatMap
            (\{ selfNode, knownNodes } ->
                withKnownStatus knownNodes |> List.map (\node -> ( selfNode, node ))
            )


unreachableLinks : Nodes -> List ( NodeAddress, NodeAddress )
unreachableLinks nodes =
    let
        withUnknownStatus : Dict NodeAddress NodeInfo -> List NodeAddress
        withUnknownStatus knownNodes =
            Dict.toList knownNodes
                |> List.filterMap
                    (\( node, info ) ->
                        case info.status of
                            UnknownNodeStatus ->
                                Just node

                            _ ->
                                if info.isUnreachable then
                                    Just node

                                else
                                    Nothing
                    )
    in
    nodes
        |> Dict.values
        |> List.concatMap
            (\{ selfNode, knownNodes } ->
                withUnknownStatus knownNodes |> List.map (\node -> ( selfNode, node ))
            )


leaders : Nodes -> List NodeAddress
leaders nodes =
    let
        filterLeaderNode : ClusterNode -> Maybe NodeAddress
        filterLeaderNode node =
            Dict.get node.selfNode node.knownNodes
                |> Maybe.andThen
                    (\v ->
                        if v.isLeader then
                            Just node.selfNode

                        else
                            Nothing
                    )
    in
    nodes
        |> Dict.values
        |> List.filterMap filterLeaderNode


leaderLinks : Nodes -> List ( NodeAddress, NodeAddress )
leaderLinks nodes =
    let
        nodeAddresses : Nodes -> List NodeAddress
        nodeAddresses nodes2 =
            nodes2
                |> Dict.values
                |> List.map .selfNode

        allLinks2 : List NodeAddress -> List ( NodeAddress, NodeAddress )
        allLinks2 ns =
            case ( List.head ns, List.tail ns ) of
                ( Just n, Just rs ) ->
                    rs
                        |> List.map (\r -> ( n, r ))
                        |> List.append (allLinks2 rs)

                _ ->
                    []
    in
    allLinks2 <| nodeAddresses nodes



------------------------------------------------------------------------------------------------------------------------


withKnownStatus : Dict NodeAddress NodeInfo -> List NodeAddress
withKnownStatus knownNodes =
    Dict.toList knownNodes
        |> List.filterMap
            (\( node, info ) ->
                case info.status of
                    UnknownNodeStatus ->
                        Nothing

                    _ ->
                        if info.isUnreachable then
                            Nothing

                        else
                            Just node
            )


sourceNode : Nodes -> NodeUrl -> Maybe NodeAddress
sourceNode nodes source =
    Maybe.map .selfNode (Dict.get source nodes)


clusterMembersToClusterNode : ClusterMembers -> ClusterNode
clusterMembersToClusterNode cm =
    let
        knownNodeAddresses : List NodeAddress
        knownNodeAddresses =
            List.map .node cm.members

        knownNodes : Dict NodeAddress NodeInfo
        knownNodes =
            Dict.fromList <| List.map (\node -> ( node, nodeInfoFromClusterMembers node cm )) knownNodeAddresses
    in
    { selfNode = cm.selfNode
    , knownNodes = knownNodes
    }


nodeInfoFromClusterMembers : NodeAddress -> ClusterMembers -> NodeInfo
nodeInfoFromClusterMembers node members =
    let
        isUnreachable : Bool
        isUnreachable =
            List.any (\u -> u.node == node && List.member members.selfNode u.observedBy) members.unreachable

        nodeStatus : Maybe MemberStatus
        nodeStatus =
            List.head <| List.map .status <| List.filter (\m -> m.node == node) members.members
    in
    { status = withDefault UnknownNodeStatus <| Maybe.map NodeStatus nodeStatus
    , isLeader = members.leader == node
    , isOldest = members.oldest == node
    , isUnreachable = isUnreachable
    }
