
import AnimationFrame
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)

screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504

main =
  Html.program { init = (init, Cmd.none)
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

type alias Model =
  { graph : Graph Entity ()
  , simulation : Force.State NodeId
  }

init : Model
init =
    let
        graph : Graph Entity ()
        graph = Graph.mapContexts nodeToEntity stringGraph

        link { from, to } = ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        { graph = graph
        , simulation = Force.simulation forces
        }

nodeToEntity : NodeContext String () -> NodeContext Entity ()
nodeToEntity = (\({ node } as ctx) ->
                   { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
               )

stringGraph = Graph.fromNodeLabelsAndEdgePairs
    [ "a"
    , "b"
    , "c"
    ]
    [ (0,1)
    , (1,2)
    , (2,0)
    ]


type alias Entity =
    Force.Entity NodeId { value : String }

type Msg = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick t -> let
                (newState, list) = Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph
                updatedGraph = updateGraphWithList model.graph list
              in (Model updatedGraph newState, Cmd.none)


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node = nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value = Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
        List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)

subscriptions : Model -> Sub Msg
subscriptions model =
    if Force.isCompleted model.simulation then
        Sub.none
    else
        AnimationFrame.times Tick


view : Model -> Svg Msg
view model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ strokeWidth "1"
            , stroke "#aaa"
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


nodeElement node =
    circle
        [ r "3.5"
        , fill "#0f0ff0"
        , stroke "transparent"
        , strokeWidth "17px"
--        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        [ Svg.title [] [ Svg.text <|  node.label.value ++ "*" ] ]
--        [ Svg.text <|  node.label.value ++ "*" ]
