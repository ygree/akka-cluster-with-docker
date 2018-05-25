import Dict exposing (Dict)
import Html exposing (Html, button, div, table, tbody, td, text, thead, tr)
import Html.Events exposing (onClick)
import Http exposing (..)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, NodeAddress, decodeMembers)
import List exposing (..)
import Maybe exposing (withDefault)
import Regex exposing (HowMany(AtMost), regex, split)
import Set exposing (Set)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (..)
import Time exposing (every, second)

main =
  Html.program { init = (model, Cmd.none)
               , view = view
               , update = update
               , subscriptions = \_ -> every second <| \_ -> Fetch
               }

-- MODEL

type alias ClusterManageUrl = String

type alias Nodes = Dict NodeAddress ClusterMembers

type alias Model = 
  { nodes : Nodes
  }

model : Model
model = { nodes = Dict.empty }


-- UPDATE

type Msg = Fetch
         | ClusterMembersResp (Result Http.Error ClusterMembers)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch -> 
      (model, Cmd.batch <| List.map getClusterMembers sourceUrls)
--      (model, withDefault Cmd.none <| List.head <| List.map getClusterMembers sourceUrls)
--      (model, getClusterMembers ("http://localhost:8558/node-" ++ toString 1 ++ "/cluster/members"))

    ClusterMembersResp (Ok result) ->
      ({ nodes = Dict.insert result.selfNode result model.nodes }, Cmd.none)

    ClusterMembersResp (Err err) ->
      (model, Cmd.none)

sourceUrls : List String
sourceUrls = List.map (\n -> "http://localhost:8558/node-" ++ toString n ++ "/cluster/members") [1,2,3,4,5]

getClusterMembers : String -> Cmd Msg
getClusterMembers url = Http.send ClusterMembersResp <| Http.get url decodeMembers -- TODO Result.mapError remove entry if error response

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text "Hello!" ]
    , button [ onClick Fetch ] [ text "Fetch" ]
    -- , div [] [ text (toString model.nodes) ]
    , viewNodes model.nodes
    ]

nodeHostname : NodeAddress -> String
nodeHostname node = withDefault node <| List.head <| List.drop 2 <| split (AtMost 3) (regex "[@:]") node


viewNodes : Nodes -> Html Msg
viewNodes nodes =
  let
    sourceNodes : List NodeAddress
    sourceNodes = List.sort <| Dict.keys nodes

    knownMembers : List ClusterMembers
    knownMembers = Dict.values nodes

    memberNodes : ClusterMembers -> List NodeAddress
    memberNodes cm = List.map .node cm.members ++ List.map .node cm.unreachable

    allNodes : Set NodeAddress
    allNodes = Set.fromList <| List.concatMap memberNodes knownMembers

    sortedAllNodes : List NodeAddress
    sortedAllNodes = List.sort <| Set.toList <| allNodes

    drawHeaderRow : List (Html Msg)
    drawHeaderRow = List.map (\nodeId -> td [] [ text <| nodeHostname nodeId ]) sortedAllNodes

    memberStatus : ClusterMember -> String
    memberStatus cm = toString cm.status


    maybeClusterMembers : NodeAddress -> NodeAddress -> Maybe ClusterMembers
    maybeClusterMembers source node = Dict.get source nodes

    maybeMemberStatus : NodeAddress -> ClusterMembers -> Maybe String
    maybeMemberStatus node cm = List.head <| List.map (\m -> toString m.status)
                                          <| List.filter (\m -> m.node == node) cm.members

    maybeUnreachable : NodeAddress -> ClusterMembers -> Maybe String
    maybeUnreachable node cm = List.head <| List.map (\_ -> "x")
                                         <| List.filter (\m -> m.node == node) cm.unreachable


    nodeStatus : NodeAddress -> NodeAddress -> Maybe String
    nodeStatus source node = Dict.get source nodes
        |> Maybe.andThen (\cm -> firstJust (maybeUnreachable node cm) (maybeMemberStatus node cm))

    drawStatus : NodeAddress -> NodeAddress -> Html Msg
    drawStatus source node = td [] [ text <| withDefault "" <| nodeStatus source node ]

    drawNodeRow : NodeAddress -> Html Msg
    drawNodeRow source = tr [] <| td [] [ text <| nodeHostname source ] :: List.map (drawStatus source) sortedAllNodes
  in
  table []
    [
      thead [] [ tr [] (td [] [ text "source" ] :: drawHeaderRow) ]
    , tbody [] (List.map drawNodeRow sourceNodes)
    ]


firstJust : Maybe a -> Maybe a -> Maybe a
firstJust x y = case x of
               Nothing -> y
               otherwise -> x

