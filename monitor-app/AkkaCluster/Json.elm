module AkkaCluster.Json exposing
  ( ClusterMembers
  , NodeAddress
  , decodeMembers
  , ClusterMember
  , MemberStatus
--  , membersSet
  )

import Set exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing
  ( decode
  , required
  )

type alias NodeAddress = String

type alias ClusterMembers =
  { selfNode : NodeAddress
  , members : List ClusterMember
  , unreachable : List ClusterUnreachableMember
  , leader : NodeAddress
  , oldest : NodeAddress
  }

type alias ClusterUnreachableMember =
  { node : NodeAddress
  -- , observedBy : List NodeAddress
  }

type MemberStatus = Joining | WeaklyUp | Up | Leaving | Exiting | Removed | Down

type alias ClusterMember =
  { node : NodeAddress
  -- nodeUid: String
  , status : MemberStatus
  -- roles: Set String
  }

node : Decoder NodeAddress
node = string

memberStatus : Decoder MemberStatus
memberStatus = string |> andThen toMemberStatus

toMemberStatus : String -> Decoder MemberStatus
toMemberStatus str =
  case str of
    "Joining"  -> succeed Joining
    "WeaklyUp" -> succeed WeaklyUp
    "Up"       -> succeed Up
    "Leaving"  -> succeed Leaving
    "Exiting"  -> succeed Exiting
    "Removed"  -> succeed Removed
    "Down"     -> succeed Down
    _ -> fail <| "Unknown MemberStatus: " ++ str

unreachable : Decoder ClusterUnreachableMember
unreachable =
  decode ClusterUnreachableMember
    |> required "node" node

member : Decoder ClusterMember
member =
  decode ClusterMember
    |> required "node" node
    |> required "status" memberStatus

decodeMembers : Decoder ClusterMembers
decodeMembers =
  -- Decode.map2 ClusterMembers 
  --   (Decode.field "selfNode" Decode.string)
  --   (Decode.field "leader" Decode.string)
  decode ClusterMembers
    |> required "selfNode" node
    |> required "members" (list member)
    |> required "unreachable" (list unreachable)
    |> required "leader" node
    |> required "oldest" node

