import Dict exposing (Dict)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button

import Html.Events exposing (onClick)
import Http exposing (..)
import AkkaCluster.Json exposing (ClusterMember, ClusterMembers, NodeAddress, decodeMembers)
import List exposing (..)
import Maybe exposing (withDefault)
import Regex exposing (HowMany(AtMost), regex, split)
import Set exposing (Set)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)

main =
  Html.program { init = (model, Cmd.none)
               , view = view
               , update = update
               , subscriptions = \_ -> Sub.none -- every second <| \_ -> Fetch
               }

-- MODEL

type alias Nodes = Dict NodeUrl ClusterMembers

type alias Model = 
  { nodes : Nodes
  }

model : Model
model = { nodes = Dict.empty
        }

-- UPDATE

type alias NodeUrl = String

type Msg = Fetch
         | ClusterMembersResp NodeUrl (Result Http.Error ClusterMembers)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch -> 
      (model, Cmd.batch <| List.map getClusterMembers sourceUrls)

    ClusterMembersResp nodeUrl (Ok result) ->
      ({ model | nodes = Dict.insert nodeUrl result model.nodes }, Cmd.none)

    ClusterMembersResp nodeUrl (Err err) ->
      ({ model | nodes = Dict.remove nodeUrl model.nodes }, Cmd.none)

sourceUrls : List NodeUrl
sourceUrls = List.map (\n -> "http://localhost:8558/node-" ++ toString n ++ "/cluster/members") [1,2,3,4,5]

getClusterMembers : NodeUrl -> Cmd Msg
getClusterMembers nodeUrl = Http.send (ClusterMembersResp nodeUrl)
                                      (Http.get nodeUrl decodeMembers)

-- VIEW

view : Model -> Html Msg
view model =
  Grid.container []
    [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
    , Grid.row []
        [ Grid.col []
          [ Button.button [ Button.primary, Button.onClick Fetch ] [ text "Fetch" ]
          -- , div [] [ text (toString model.nodes) ]
          , viewNodes model.nodes
          ]
        ]
    ]


nodeHostname : NodeAddress -> String
nodeHostname node = withDefault node <| List.head <| List.drop 2 <| split (AtMost 3) (regex "[@:]") node


viewNodes : Nodes -> Html Msg
viewNodes nodes =
  let
    sourceNodes : List NodeUrl
    sourceNodes = List.sortBy sourceHostname <| Dict.keys nodes

    knownMembers : List ClusterMembers
    knownMembers = Dict.values nodes

    memberNodes : ClusterMembers -> List NodeAddress
    memberNodes cm = List.map .node cm.members ++ List.map .node cm.unreachable

    allNodes : Set NodeAddress
    allNodes = Set.fromList <| List.concatMap memberNodes knownMembers

    sortedAllNodes : List NodeAddress
    sortedAllNodes = List.sort <| Set.toList <| allNodes

    maybeClusterMembers : NodeUrl -> Maybe ClusterMembers
    maybeClusterMembers source = Dict.get source nodes

    maybeMemberStatus : NodeAddress -> ClusterMembers -> Maybe String
    maybeMemberStatus node cm = List.head <| List.map (\m -> toString m.status)
                                          <| List.filter (\m -> m.node == node) cm.members

    maybeUnreachable : NodeAddress -> ClusterMembers -> Maybe String
    maybeUnreachable node cm = List.head <| List.map (\_ -> "x")
                                         <| List.filter (\m -> m.node == node) cm.unreachable

    sourceNode : NodeUrl -> Maybe NodeAddress
    sourceNode source = Maybe.map (.selfNode) (maybeClusterMembers source)

    sourceHostname : NodeUrl -> String
    sourceHostname source = withDefault source <| Maybe.map nodeHostname (sourceNode source)

    nodeStatus : NodeUrl -> NodeAddress -> Maybe String
    nodeStatus source node = maybeClusterMembers source
        |> Maybe.andThen (\cm -> firstJust (maybeUnreachable node cm) (maybeMemberStatus node cm))

    drawNodeCell : NodeUrl -> NodeAddress -> Table.Cell Msg
    drawNodeCell source node =
      let
        leaderLabel : Maybe String
        leaderLabel = Maybe.andThen (\cm -> if cm.leader == node then Just "leader" else Nothing) (maybeClusterMembers source)

        oldestLabel : Maybe String
        oldestLabel = Maybe.andThen (\cm -> if cm.oldest == node then Just "oldest" else Nothing) (maybeClusterMembers source)

        labels = foldr (++) "" <| intersperse " | " <| maybeToList leaderLabel ++ maybeToList oldestLabel
      in
      Table.td []
        [ div [] [ text <| withDefault "" <| nodeStatus source node ]
        , div [] [ text labels ]
        ]

    nodeTableHeaders : List (Html Msg)
    nodeTableHeaders = List.map (\nodeId -> text <| nodeHostname nodeId) sortedAllNodes

    drawNodeRow : NodeUrl -> Table.Row Msg
    drawNodeRow source = Table.tr [] <| Table.td [ Table.cellAttr <| title source ] [ text <| sourceHostname source ] :: List.map (drawNodeCell source) sortedAllNodes
  in
  Table.table
    { options = [ Table.striped, Table.hover, Table.small, Table.bordered ]
    , thead = Table.simpleThead <|
        List.map (\v -> Table.th [] [v]) (text "source" :: nodeTableHeaders)
    , tbody = Table.tbody [] (List.map drawNodeRow sourceNodes)
    }


firstJust : Maybe a -> Maybe a -> Maybe a
firstJust x y = case x of
               Nothing -> y
               otherwise -> x

maybeToList : Maybe a -> List a
maybeToList m = withDefault [] <| Maybe.map (\x -> [x]) m

