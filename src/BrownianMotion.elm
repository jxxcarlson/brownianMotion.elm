module Main exposing (..)

-- On commandshttps://www.elm-tutorial.org/en/03-subs-cmds/02-commands.html

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class, id)
import Time exposing (Time, second)
import Svg exposing (svg, circle)
import Svg.Attributes as SA exposing (cx, cy, fill, width, height, r)
import Random
import Graph
    exposing
        ( drawPointList
        , drawIntegerTimeSeries
        , drawLine
        , Color
        , Circle
        , renderHistory
        )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type SimulatorState
    = Running
    | Paused
    | Start


rgb : Color -> String
rgb color =
    "red"



-- MODEL


type alias Model =
    { simulatorState : SimulatorState
    , count : Int
    , x_max : Float
    , y_max : Float
    , currentCircle : Circle
    , graphData : Graph.GraphData
    , history : List Circle
    , message : String
    , info : String
    }



-- start : RunMode -> ( Model, Cmd Msg )


init : ( Model, Cmd Msg )
init =
    let
        x_max =
            100.0

        y_max =
            100.0

        x =
            x_max / 2

        y =
            y_max / 2

        radius =
            0.6

        color =
            Color 255 0 0 0.5

        circle =
            Circle x y radius color

        source =
            Graph.Rect 0.0 0.0 x_max y_max

        target =
            Graph.Rect 0.0 0.0 500.0 500.0

        graphData =
            Graph.GraphData source target "black" "white"

        history =
            [ circle ]

        message =
            "n: 0, x: " ++ (toString x) ++ ", y: " ++ (toString y) ++ ", distance: 0"
    in
        ( Model Start
            0
            x_max
            y_max
            circle
            graphData
            []
            message
            ""
        , Cmd.none
        )


referenceCircle : Circle
referenceCircle =
    let
        ( m, cmd ) =
            init
    in
        Circle (m.x_max / 2) (m.y_max / 2) (2 * m.currentCircle.r) (Color 0 255 255 0.5)



-- UPDATE


type Msg
    = MakeMove ( Int, Int )
    | Reset
    | Pause
    | Run
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Pause ->
            handlePause model

        Run ->
            ( { model | simulatorState = Running }, Cmd.none )

        Tick newTime ->
            if model.simulatorState == Running then
                ( model, Random.generate MakeMove randomMove )
            else
                ( model, Cmd.none )

        MakeMove ( a, b ) ->
            if model.simulatorState == Running then
                update_model ( a, b ) model
            else
                ( model, Cmd.none )


randomMove : Random.Generator ( Int, Int )
randomMove =
    Random.pair (Random.int 0 10) (Random.int 0 10)


handlePause : Model -> ( Model, Cmd Msg )
handlePause model =
    let
        newSimulatorState =
            if model.simulatorState == Paused then
                Running
            else
                Paused
    in
        ( { model | simulatorState = newSimulatorState }, Cmd.none )


update_model : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
update_model ( a, b ) model =
    let
        new_count =
            model.count + 1

        currentCircle =
            model.currentCircle

        k =
            1.0

        x =
            currentCircle.x + 2 * k * (toFloat a / 10) - 1

        y =
            currentCircle.y + 2 * k * (toFloat b / 10) - 1

        r =
            currentCircle.r

        color =
            currentCircle.color

        new_info =
            if x < r || x > model.x_max - r || y < r || y > model.y_max - r then
                "COLLISION !"
            else
                ""

        new_bgColor =
            if x < r || x > model.x_max - r || y < r || y > model.y_max - r then
                "#EEEE22"
            else
                "black"

        oldGraphData =
            model.graphData

        newGraphData =
            { oldGraphData | bgColor = new_bgColor }

        rebound_factor =
            3.0

        x_new =
            if x < r then
                k * rebound_factor * r
            else if x > model.x_max - r then
                model.x_max - k * rebound_factor * r
            else
                x

        y_new =
            if y < r then
                k * rebound_factor * r
            else if y > model.y_max - r then
                model.y_max - k * rebound_factor * r
            else
                y

        newCircle =
            Circle x_new y_new currentCircle.r color

        new_history =
            model.history ++ [ newCircle ]

        dx =
            newCircle.x - model.x_max / 2

        dy =
            newCircle.y - model.y_max / 2

        d_squared =
            dx * dx + dy * dy

        distance =
            round (sqrt d_squared)

        new_message =
            "n: " ++ (toString model.count) ++ ", x: " ++ (toString (round newCircle.x)) ++ ", y: " ++ (toString (round newCircle.y)) ++ ", distance: " ++ (toString distance)
    in
        ( { model | graphData = newGraphData, count = new_count, currentCircle = newCircle, history = new_history, message = new_message, info = new_info }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (50 * Time.millisecond) Tick


moderate_circle : Float -> Circle -> Circle
moderate_circle k circle =
    let
        cc =
            circle.color

        newColor =
            { cc | a = k * cc.a }
    in
        { circle | color = newColor }



-- moderate_history : Float -> List Circle -> List Circle
-- moderate_history k circle_list =
-- VIEW


view : Model -> Html Msg
view model =
    div [ id "graphics_area" ]
        [ svg
            [ SA.width "500", SA.height "500" ]
            ([ (Graph.boundingRect model.graphData)
             , (Graph.drawCircle model.graphData referenceCircle)
               --, (Graph.drawCircle model.graphData model.currentCircle
             ]
                ++ (renderHistory model.graphData model.history)
            )
        , br [] []
        , button [ onClick Run, id "run" ] [ text "Run" ]
        , button [ onClick Pause, id "pause" ] [ text "Pause" ]
        , button [ onClick Reset, id "reset" ] [ text "Reset" ]
        , span [ id "message" ] [ text model.message ]
        , br [] []
        , br [] []
        , span [ id "info" ] [ text model.info ]
        ]
