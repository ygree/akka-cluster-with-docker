module AkkaCluster.Graph exposing
  ( GraphNodes
  , emptyGraphNodes
  , updateGraphNodes
  )

import AkkaCluster.Json exposing (NodeAddress)
import AkkaCluster.Nodes exposing (..)
import Dict exposing (Dict)
import Graph exposing (Graph, NodeId)
import Set
import Visualization.Force as Force exposing (State)


type alias GraphNodes =
  { nodeMap : NodeMap
--  , graph : Graph Entity ()
  , simulation : Force.State NodeId
  }

type alias NodeMap = Dict NodeAddress NodeId

type alias Entity =
  Force.Entity NodeId { value : String }

emptyGraphNodes : GraphNodes
emptyGraphNodes =
  { nodeMap = Dict.empty
--  , graph = Graph.empty
  , simulation = Force.simulation []
  }

updateGraphNodes : GraphNodes -> Nodes -> GraphNodes
updateGraphNodes graphNodes nodes =
  let
    newNodeMap = updateNodesMap graphNodes.nodeMap (sortedAllNodes nodes)
--    graph = graphNodes.graph
    simulation = graphNodes.simulation
  in
    { nodeMap = newNodeMap
--    , graph = graph -- TODO: update graph based on the diff of graphNodes.nodeMap and newNodeMap
    , simulation = simulation -- TODO: update simulation
    }

updateNodesMap : NodeMap -> List NodeAddress -> NodeMap
updateNodesMap nodeMap nodes =
  let
    existingNodes = Dict.keys nodeMap |> Set.fromList
    maxNodeId = Dict.values nodeMap |> List.maximum |> Maybe.withDefault 0
    setOfNodes = Set.fromList nodes
    newNodes = Set.diff setOfNodes existingNodes

    newNodePairs : List (NodeId, NodeAddress)
    newNodePairs = Tuple.second <|
      Set.foldr (\nodeAddr (nextId, result) -> (nextId + 1, (nextId, nodeAddr) :: result))
        (maxNodeId + 1, []) newNodes

    updatedNodeMap = List.foldr (\(nodeId, nodeAddr) nodeMap -> Dict.insert nodeAddr nodeId nodeMap) nodeMap newNodePairs
  in
    updatedNodeMap

