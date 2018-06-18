module AkkaCluster.Graph exposing
  ( GraphNodes
  , Entity
  , NodesLink
  , emptyGraphNodes
  , updateGraphNodes
  , screenWidth
  , screenHeight
  )

import AkkaCluster.Json exposing (NodeAddress)
import AkkaCluster.Nodes as Nodes exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Set exposing (Set)
import Visualization.Force as Force exposing (State)


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


type alias GraphNodes =
  { entities : List Entity
  , links : Set NodesLink
  , simulation : Force.State NodeAddress
  }

type alias Entity = Force.Entity NodeAddress { value : NodeInfo }

type alias NodesLink = (NodeAddress, NodeAddress)

nodeLink : NodeAddress -> NodeAddress -> NodesLink
nodeLink a b = if a <= b
               then (a, b)
               else (b, a)

emptyGraphNodes : GraphNodes
emptyGraphNodes =
  { entities = []
  , links = Set.empty
  , simulation = Force.simulation []
  }

updateGraphNodes : GraphNodes -> Nodes -> GraphNodes
updateGraphNodes { entities, links, simulation } nodes =
  let
    allNodes : Dict NodeAddress Nodes.NodeInfo
    allNodes = Nodes.allNodeInfo nodes

    newNodes : Set NodeAddress
    newNodes = Set.diff (Set.fromList <| Dict.keys allNodes)
                        (entities |> List.map .id |> Set.fromList)

    missingNodes : Set NodeAddress
    missingNodes = Set.diff (entities |> List.map (\({id}) -> id) |> Set.fromList)
                            (Set.fromList <| Dict.keys allNodes)

    leftEntities : List Entity
    leftEntities = entities |> List.filter (\e -> not (Set.member e.id missingNodes))

    -- TODO: depict new nodes in the center
    startingIndex = List.length leftEntities

    newEntities : List Entity
    newEntities = newNodes |> Set.toList
                           |> List.indexedMap (,)
                           |> List.filterMap (\(idx, nodeAddr) -> Maybe.map (\v -> Force.entity (startingIndex + idx) (nodeAddr, v)) (Dict.get nodeAddr allNodes))
                           |> List.map (\e -> { e | id = Tuple.first e.value, value = Tuple.second e.value })

    updatedEntities : List Entity
    updatedEntities = leftEntities |> List.map (\e -> { e | value = withDefault e.value (Dict.get e.id allNodes) })

    allEntities : List Entity
    allEntities = List.append updatedEntities newEntities

    nodeLinks : Set NodesLink
    nodeLinks = Nodes.allLinks nodes |> List.filter (\(n, m) -> n /= m)
                                     |> List.map (uncurry nodeLink)
                                     |> Set.fromList

    unreachableLinks : Set NodesLink
    unreachableLinks = Nodes.unreachableLinks nodes |> List.filter (\(n, m) -> n /= m)
                                                    |> List.map (uncurry nodeLink)
                                                    |> Set.fromList

    leaderLinks : Set NodesLink
    leaderLinks = Nodes.leaderLinks nodes |> Set.fromList

    visibleLinks = forceLinks 100 <| Set.toList nodeLinks
    invisibleLinks = forceLinks 250 <| Set.toList leaderLinks

    forces =
        [
          visibleLinks
        , invisibleLinks
        , Force.manyBodyStrength -15 (Dict.keys allNodes)
        , Force.center (screenWidth / 2) (screenHeight / 2)
        ]

    forceLinks : Float -> List ( comparable, comparable ) -> Force.Force comparable
    forceLinks distance =
      List.map (\( source, target ) -> { source = source, target = target, distance = distance, strength = Nothing }) >> Force.customLinks 1

--    Simulation oldSimulation =

  in
    { entities = allEntities
    , links = nodeLinks
    , simulation = Force.simulation forces
    }



