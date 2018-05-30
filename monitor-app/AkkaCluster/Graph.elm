module AkkaCluster.Graph exposing
  ( GraphNodes
  , emptyGraphNodes
  , updateGraphNodes
  )

import AkkaCluster.Json exposing (NodeAddress)
import AkkaCluster.Nodes exposing (..)
import Dict exposing (Dict)
import Graph exposing (Graph, NodeId)
import Set exposing (Set)
import Visualization.Force as Force exposing (State)


type alias GraphNodes =
  { entities : List Entity
  , links : Set NodesLink
--  , simulation : Force.State NodeId
  }

type alias Entity = Force.Entity NodeAddress { value : NodeAddress }

type alias NodesLink = (NodeAddress, NodeAddress)

nodeLink : NodeAddress -> NodeAddress -> NodesLink
nodeLink a b = if a <= b
               then (a, b)
               else (b, a)

emptyGraphNodes : GraphNodes
emptyGraphNodes =
  { entities = []
  , links = Set.empty
  }

updateGraphNodes : GraphNodes -> Nodes -> GraphNodes
updateGraphNodes { entities, links } nodes =
  let
    newNodeMap : Set NodeAddress
    newNodeMap = Set.diff
                   (nodes |> allNodes |> Set.fromList)
                   (entities |> List.map .id |> Set.fromList)

    startingIndex = List.length entities

    newEntities : List Entity
    newEntities = newNodeMap |> Set.toList
                             |> List.indexedMap (,)
                             |> List.map (\(idx, nodeAddr) -> Force.entity (startingIndex + idx) nodeAddr)
                             |> List.map (\e -> { e | id = e.value })

  in
    { entities = List.append entities newEntities -- TODO exclude removed nodes
    , links = links -- TODO links
    }



