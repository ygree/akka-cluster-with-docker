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
  , nodeStatus
  , maybeClusterMembers
  , isLeader
  , isOldest
  )

import Dict exposing (Dict)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, NodeAddress, decodeMembers)
import Maybe exposing (withDefault)
import Regex exposing (HowMany(AtMost), regex, split)
import Set exposing (Set)

type alias Nodes = Dict NodeUrl ClusterMembers

empty : Nodes
empty = Dict.empty

type alias NodeUrl = String

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

isLeader : Nodes -> NodeUrl -> NodeAddress -> Bool
isLeader nodes source node = withDefault False <| Maybe.map (\v -> v.leader == node) (maybeClusterMembers nodes source)

isOldest : Nodes -> NodeUrl -> NodeAddress -> Bool
isOldest nodes source node = withDefault False <| Maybe.map (\v -> v.oldest == node) (maybeClusterMembers nodes source)

maybeClusterMembers : Nodes -> NodeUrl -> Maybe ClusterMembers
maybeClusterMembers nodes source = Dict.get source nodes

maybeMemberStatus : NodeAddress -> ClusterMembers -> Maybe String
maybeMemberStatus node cm = List.head <| List.map (\m -> toString m.status)
                                      <| List.filter (\m -> m.node == node) cm.members

maybeUnreachable : NodeAddress -> ClusterMembers -> Maybe String
maybeUnreachable node cm = List.head <| List.map (\_ -> "x")
                                     <| List.filter (\m -> m.node == node) cm.unreachable

sourceHostname : Nodes -> NodeUrl -> String
sourceHostname nodes source = withDefault source <| Maybe.map nodeHostname (sourceNode nodes source)

nodeStatus : Nodes -> NodeUrl -> NodeAddress -> Maybe String
nodeStatus nodes source node = maybeClusterMembers nodes source
    |> Maybe.andThen (\cm -> firstJust (maybeUnreachable node cm) (maybeMemberStatus node cm))

------------------------------------------------------------------------------------------------------------------------

sourceNode : Nodes -> NodeUrl -> Maybe NodeAddress
sourceNode nodes source = Maybe.map (.selfNode) (maybeClusterMembers nodes source)

firstJust : Maybe a -> Maybe a -> Maybe a
firstJust x y = case x of
               Nothing -> y
               otherwise -> x

