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
  , unreachableLinks : Set NodesLink
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
  , unreachableLinks = Set.empty
  }

updateGraphNodes : GraphNodes -> Nodes -> GraphNodes
updateGraphNodes { entities, links, simulation, unreachableLinks } nodes =
  let
    allNodeInfos : Dict NodeAddress Nodes.NodeInfo
    allNodeInfos = Nodes.allNodeInfo nodes

    allNodes : Set NodeAddress
    allNodes = Nodes.allNodes nodes

    newNodes : Set NodeAddress
    newNodes = Set.diff allNodes
                        (entities |> List.map .id |> Set.fromList)

    missingNodes : Set NodeAddress
    missingNodes = Set.diff (entities |> List.map (\({id}) -> id) |> Set.fromList)
                            allNodes

    leftEntities : List Entity
    leftEntities = entities |> List.filter (\e -> not (Set.member e.id missingNodes))

    -- TODO: depict new nodes in the center
    startingIndex = List.length leftEntities

    unknownNodeInfo : NodeInfo
    unknownNodeInfo =
      { status = UnknownNodeStatus
      , isLeader = False
      , isOldest = False
      , isUnreachable = True
      }

    newEntities : List Entity
    newEntities = newNodes |> Set.toList
                           |> List.indexedMap (,)
                           |> List.map (\(idx, nodeAddr) ->
                                Force.entity (startingIndex + idx)
                                          ( nodeAddr
                                          , withDefault unknownNodeInfo <| Dict.get nodeAddr allNodeInfos
                                          )
                              )
                           |> List.map (\e -> { e | id = Tuple.first e.value, value = Tuple.second e.value })

    updatedEntities : List Entity
    updatedEntities = leftEntities |> List.map (\e -> { e | value = withDefault e.value (Dict.get e.id allNodeInfos) })

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
        , Force.manyBodyStrength -15 (Set.toList allNodes)
        , Force.center (screenWidth / 2) (screenHeight / 2)
        ]

    forceLinks : Float -> List ( comparable, comparable ) -> Force.Force comparable
    forceLinks distance =
      List.map (\( source, target ) -> { source = source, target = target, distance = distance, strength = Nothing }) >> Force.customLinks 1

--    Simulation oldSimulation =

  in
    { entities = allEntities
    , links = nodeLinks
    , unreachableLinks = unreachableLinks
    , simulation = Force.simulation forces
    }



