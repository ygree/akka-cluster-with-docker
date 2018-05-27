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
import AnimationFrame
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Svg
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)
import AkkaCluster.Nodes as Nodes exposing (Nodes, NodeUrl)

main =
  Html.program { init = (model, Cmd.none)
               , view = view
               , update = update
               , subscriptions = \_ -> Sub.none -- every second <| \_ -> Fetch
               }

-- MODEL

type alias Model =
  { nodes : Nodes
  }

model : Model
model = { nodes = Dict.empty
        }

-- UPDATE

type Msg = Fetch
         | ClusterMembersResp NodeUrl (Result Http.Error ClusterMembers)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch -> 
      (model, Cmd.batch <| List.map getClusterMembers sourceUrls)

    ClusterMembersResp nodeUrl (Ok result) ->
      ({ model | nodes = Nodes.insertClusterMembers model.nodes nodeUrl result }, Cmd.none)

    ClusterMembersResp nodeUrl (Err err) ->
      ({ model | nodes = Nodes.removeClusterMembers model.nodes nodeUrl }, Cmd.none)

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

viewNodes : Nodes -> Html Msg
viewNodes nodes =
  let
    drawNodeCell : NodeUrl -> NodeAddress -> Table.Cell Msg
    drawNodeCell source node =
      let
        sourceNodes = Nodes.maybeClusterMembers nodes source

        leaderLabel : Maybe String
        leaderLabel = Maybe.andThen (\cm -> if cm.leader == node then Just "leader" else Nothing) sourceNodes

        oldestLabel : Maybe String
        oldestLabel = Maybe.andThen (\cm -> if cm.oldest == node then Just "oldest" else Nothing) sourceNodes

        labels = foldr (++) "" <| intersperse " | " <| maybeToList leaderLabel ++ maybeToList oldestLabel
      in
      Table.td []
        [ div [] [ text <| withDefault "" <| Nodes.nodeStatus nodes source node ]
        , div [] [ text labels ]
        ]

    nodeTableHeaders : List (Html Msg)
    nodeTableHeaders = List.map (\nodeId -> text <| Nodes.nodeHostname nodeId) <| Nodes.sortedAllNodes nodes

    drawNodeRow : NodeUrl -> Table.Row Msg
    drawNodeRow source = Table.tr [] <| Table.td [ Table.cellAttr <| title source ] [ text <| Nodes.sourceHostname nodes source ] :: List.map (drawNodeCell source) (Nodes.sortedAllNodes nodes)
  in
  Table.table
    { options = [ Table.striped, Table.hover, Table.small, Table.bordered ]
    , thead = Table.simpleThead <| List.map (\v -> Table.th [] [v]) (text "source" :: nodeTableHeaders)
    , tbody = Table.tbody [] (List.map drawNodeRow <| Nodes.sourceNodes nodes)
    }

maybeToList : Maybe a -> List a
maybeToList m = withDefault [] <| Maybe.map (\x -> [x]) m

