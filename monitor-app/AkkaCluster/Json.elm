module AkkaCluster.Json exposing
    ( ClusterMember
    , ClusterMembers
    , MemberStatus
    , NodeAddress
    , decodeMembers
    )

import Json.Decode exposing (..)
import Json.Decode.Pipeline
    exposing
        ( required
        )
import Set exposing (..)


type alias NodeAddress =
    String


type alias ClusterMembers =
    { selfNode : NodeAddress
    , members : List ClusterMember
    , unreachable : List ClusterUnreachableMember
    , leader : NodeAddress
    , oldest : NodeAddress
    }


type alias ClusterUnreachableMember =
    { node : NodeAddress
    , observedBy : List NodeAddress
    }


type MemberStatus
    = Joining
    | WeaklyUp
    | Up
    | Leaving
    | Exiting
    | Removed
    | Down


type alias ClusterMember =
    { node : NodeAddress

    -- nodeUid: String
    , status : MemberStatus

    -- roles: Set String
    }


node : Decoder NodeAddress
node =
    string


memberStatus : Decoder MemberStatus
memberStatus =
    string |> andThen toMemberStatus


toMemberStatus : String -> Decoder MemberStatus
toMemberStatus str =
    case str of
        "Joining" ->
            succeed Joining

        "WeaklyUp" ->
            succeed WeaklyUp

        "Up" ->
            succeed Up

        "Leaving" ->
            succeed Leaving

        "Exiting" ->
            succeed Exiting

        "Removed" ->
            succeed Removed

        "Down" ->
            succeed Down

        _ ->
            fail <| "Unknown MemberStatus: " ++ str


unreachable : Decoder ClusterUnreachableMember
unreachable =
    succeed ClusterUnreachableMember
        |> required "node" node
        |> required "observedBy" (list node)


member : Decoder ClusterMember
member =
    succeed ClusterMember
        |> required "node" node
        |> required "status" memberStatus


decodeMembers : Decoder ClusterMembers
decodeMembers =
    succeed ClusterMembers
        |> required "selfNode" node
        |> required "members" (list member)
        |> required "unreachable" (list unreachable)
        |> required "leader" node
        |> required "oldest" node
