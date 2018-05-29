module AkkaCluster.Nodes exposing
  ( Nodes
  , empty
  , NodeUrl
  , insertClusterMembers
  , removeClusterMembers
  , nodeHostname
  , sourceNodes
  , sourceHostname
  , sortedAllNodes
  , maybeClusterMembers
  , nodeInfo
  , NodeStatus (..)
  )

import Dict exposing (Dict)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, MemberStatus, NodeAddress, decodeMembers)
import Maybe exposing (withDefault)
import Regex exposing (HowMany(AtMost), regex, split)
import Set exposing (Set)
import AkkaCluster.Json

type alias Nodes = Dict NodeUrl ClusterMembers

empty : Nodes
empty = Dict.empty

type alias NodeUrl = String

type NodeStatus = NodeStatus AkkaCluster.Json.MemberStatus
                | UnknownNodeStatus

type alias NodeInfo =
  { status : NodeStatus
  , isLeader : Bool
  , isOldest : Bool
  }

insertClusterMembers : Nodes -> NodeUrl -> ClusterMembers -> Nodes
insertClusterMembers nodes nodeUrl clusterMembers = Dict.insert nodeUrl clusterMembers nodes

removeClusterMembers : Nodes -> NodeUrl -> Nodes
removeClusterMembers nodes nodeUrl = Dict.remove nodeUrl nodes


nodeHostname : NodeAddress -> String
nodeHostname node = withDefault node <| List.head <| List.drop 2 <| split (AtMost 3) (regex "[@:]") node

sourceNodes : Nodes -> List NodeUrl
sourceNodes nodes = List.sortBy (sourceHostname nodes) (Dict.keys nodes)

knownMembers : Nodes -> List ClusterMembers
knownMembers nodes = Dict.values nodes

memberNodes : ClusterMembers -> List NodeAddress
memberNodes cm = List.map .node cm.members ++ List.map .node cm.unreachable

allNodes : Nodes -> Set NodeAddress
allNodes nodes = Set.fromList <| List.concatMap memberNodes <| knownMembers nodes

sortedAllNodes : Nodes -> List NodeAddress
sortedAllNodes nodes = List.sort <| Set.toList <| allNodes nodes

maybeClusterMembers : Nodes -> NodeUrl -> Maybe ClusterMembers
maybeClusterMembers nodes source = Dict.get source nodes

maybeMemberStatus : NodeAddress -> ClusterMembers -> Maybe String
maybeMemberStatus node cm = List.head <| List.map (\m -> toString m.status)
                                      <| List.filter (\m -> m.node == node) cm.members

sourceHostname : Nodes -> NodeUrl -> String
sourceHostname nodes source = withDefault source <| Maybe.map nodeHostname (sourceNode nodes source)

nodeInfo : Nodes -> NodeUrl -> NodeAddress -> Maybe NodeInfo
nodeInfo nodes source node = Maybe.map (nodeInfoFromClusterMembers node) (maybeClusterMembers nodes source)

------------------------------------------------------------------------------------------------------------------------

sourceNode : Nodes -> NodeUrl -> Maybe NodeAddress
sourceNode nodes source = Maybe.map (.selfNode) (maybeClusterMembers nodes source)

firstJust : Maybe a -> Maybe a -> Maybe a
firstJust x y = case x of
               Nothing -> y
               otherwise -> x

nodeInfoFromClusterMembers : NodeAddress -> ClusterMembers -> NodeInfo
nodeInfoFromClusterMembers node members =
  let
    isUnreachable : Bool
    isUnreachable = List.any (\u -> u.node == node && List.member members.selfNode u.observedBy) members.unreachable

    nodeStatus : Maybe MemberStatus
    nodeStatus = List.head <| List.map (.status) <| List.filter (\m -> m.node == node) members.members
  in
    { status = if isUnreachable
               then UnknownNodeStatus
               else withDefault UnknownNodeStatus <| Maybe.map NodeStatus nodeStatus
    , isLeader = members.leader == node
    , isOldest = members.oldest == node
    }


