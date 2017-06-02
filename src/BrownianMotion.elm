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



-- MODEL


type alias Model =
    { simulatorState : SimulatorState
    , count : Int
    , x_max : Float
    , y_max : Float
    , x : Float
    , y : Float
    , radius : Float
    , graphData : Graph.GraphData
    , message : String
    , info : String
    }



-- start : RunMode -> ( Model, Cmd Msg )


init : ( Model, Cmd Msg )
init =
    let
        x_max =
            20.0

        y_max =
            20.0

        x =
            x_max / 2

        y =
            y_max / 2

        radius =
            2.0

        source =
            Graph.Rect 0.0 0.0 x_max y_max

        target =
            Graph.Rect 0.0 0.0 500.0 500.0

        graphData =
            Graph.GraphData source target "#D3E8DD" "white"

        message =
            "n: 0, x: " ++ (toString x) ++ ", y: " ++ (toString y) ++ ", distance: 0"
    in
        ( Model Start
            0
            x_max
            y_max
            x
            y
            radius
            graphData
            message
            ""
        , Cmd.none
        )



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
                update_position ( a, b ) model
            else
                ( model, Cmd.none )


randomMove : Random.Generator ( Int, Int )
randomMove =
    Random.pair (Random.int 0 1) (Random.int 0 1)


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


update_position : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
update_position ( a, b ) model =
    let
        new_count =
            model.count + 1

        x =
            model.x + 2 * (toFloat a) - 1

        y =
            model.y + 2 * (toFloat b) - 1

        r =
            model.radius

        new_info =
            if x < r || x > model.x_max - r || y < r || y > model.y_max - r then
                "COLLISION !"
            else
                ""

        new_bgColor =
            if x < r || x > model.x_max - r || y < r || y > model.y_max - r then
                "#EEEE22"
            else
                "#D3E8DD"

        oldGraphData =
            model.graphData

        newGraphData =
            { oldGraphData | bgColor = new_bgColor }

        rebound_factor =
            1.1

        x_new =
            if x < r then
                rebound_factor * r
            else if x > model.x_max - r then
                model.x_max - rebound_factor * r
            else
                x

        y_new =
            if y < r then
                rebound_factor * r
            else if y > model.y_max - r then
                model.y_max - rebound_factor * r
            else
                y

        dx =
            model.x - model.x_max / 2

        dy =
            model.y - model.y_max / 2

        d_squared =
            dx * dx + dy * dy

        distance =
            round (sqrt d_squared)

        new_message =
            "n: " ++ (toString model.count) ++ ", x: " ++ (toString (round model.x)) ++ ", y: " ++ (toString (round model.y)) ++ ", distance: " ++ (toString distance)
    in
        ( { model | graphData = newGraphData, count = new_count, x = x_new, y = y_new, message = new_message, info = new_info }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (200 * Time.millisecond) Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "graphics_area" ]
        [ svg
            [ SA.width "500", SA.height "500" ]
            [ (Graph.boundingRect model.graphData)
            , (Graph.drawCircle model.graphData "none" "red" 0.6 model.x model.y model.radius)
            , (Graph.drawCircle model.graphData "none" "blue" 0.25 (model.x_max / 2) (model.y_max / 2) (model.radius * 2))
            ]
        , br [] []
        , button [ onClick Run, id "run" ] [ text "Run" ]
        , button [ onClick Pause, id "pause" ] [ text "Pause" ]
        , button [ onClick Reset, id "reset" ] [ text "Reset" ]
        , span [ id "message" ] [ text model.message ]
        , br [] []
        , br [] []
        , span [ id "info" ] [ text model.info ]
        ]
