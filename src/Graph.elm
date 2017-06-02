module Graph exposing (..)

{-| Module Graph provides a set of functions for
constructing simple graphs, give a sequence of points
like

  data = [(0.0, 0.0), (5.0, 7.0), (8.0, 2.0), ...]

or a "time series" like

  data = [1, 2, 4, 9, 7, ...],

where we assume the time intervals between data points are equal.

The main functions are

  (1)  Graph.drawPointList graphData "yellow" data

  (2)  Graph.drawTimeSeries "blue" graphData data

  (3)  Graph.drawIntegerTimeSeries "blue" graphData data

where (1) is for sequences of points, (2) is for seqeunces of integers,
and (3) is for sequences of floats.

graphData is a structure that defines two rectangles, sourceRect and
targetRect.  The first should be though of as being in the Cartesian
plane, the second on the computer screen.  The information in graphData
is used to define a mapping from the first rectangle to the second.

-}

import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)


{-| The three data structures below define
  the needed geometric structures: points,
  lists of points, and rectangles.
-}
type alias Point =
    ( Float, Float )


type alias Points =
    List Point


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height :
        Float
    }


{-| GraphData is as in the introduction: two rectangles
  plus some additional data (colors)
-}
type alias GraphData =
    { sourceRect : Rect
    , targetRect : Rect
    , bgColor : String
    , rectStrokeColor : String
    }


{-| boundingRect returns an SVG representation of
  the targetRect in graphData.
-}
boundingRect : GraphData -> Svg msg
boundingRect graphData =
    S.rect
        [ x (toString graphData.targetRect.x)
        , y (toString graphData.targetRect.y)
        , width (toString graphData.targetRect.width)
        , height (toString graphData.targetRect.height)
        , fill graphData.bgColor
        , stroke graphData.rectStrokeColor
        ]
        []


{-| AffineTransformData carries the coefficients of an
  affine transformation
    xx = ax + b
    yy= cy + d
-}
type alias AffineTransformData =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    }


{-| affineTransformData takes a GraphData object and
  returns an AffineTransformData object such that the
  associated affine transformation maps sourceRect to
  targetRect.
-}
affineTransformData : GraphData -> AffineTransformData
affineTransformData graphData =
    let
        aa =
            graphData.targetRect.width / graphData.sourceRect.width

        bb =
            graphData.targetRect.x - graphData.sourceRect.x

        cc =
            -graphData.targetRect.height / graphData.sourceRect.height

        dd =
            graphData.targetRect.y - graphData.sourceRect.y + graphData.targetRect.height
    in
        { a = aa, b = bb, c = cc, d = dd }


{-| affineTransformPoint affineTransformData is
  an affine transformation.
-}
affineTransformPoint : AffineTransformData -> Point -> Point
affineTransformPoint affineTransformData point =
    let
        x =
            Tuple.first point

        y =
            Tuple.second point

        xx =
            affineTransformData.a * x + affineTransformData.b

        yy =
            affineTransformData.c * y + affineTransformData.d
    in
        ( xx, yy )


{-| affineTransformPoints affineTransformData is a function
  that applies an affine transformation to a list of points,
  returning a new list of points.
-}
affineTransformPoints : AffineTransformData -> Points -> Points
affineTransformPoints affineTransformData points =
    let
        f =
            affineTransformPoint affineTransformData
    in
        List.map f points


{-| zip [a, b, c] [1, 2, 3] = [(a,1), (b,2), (c,3)]
  The (,) expression is a shortcut to create 2-tuples, so
  evaluating ((,) 3 4) results in (3,4)
-}
zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)


{-| timeSeries [0.4, 1.3, 2.9] = [(0, 0.4), (1, 1.3), (2, 2.9)]
-}
timeSeries : List Float -> List ( Float, Float )
timeSeries data =
    let
        n =
            List.length data

        timeSequence =
            List.range 0 (n - 1) |> List.map toFloat
    in
        zip timeSequence data


{-| point2String (1.2, 2.7) == "1.2, 2.7"
-}
point2String : Point -> String
point2String point =
    (toString (Tuple.first point)) ++ ", " ++ (toString (Tuple.second point))


{-| data2SVG GraphData [(0, 2), (1,4), (2,3)]
  => "0,2  1,4  2,3"
  data2SVG : GraphData -> Svg Msg
-}
data2SVG : GraphData -> Points -> String
data2SVG graphData data =
    let
        affData =
            affineTransformData graphData

        aff =
            affineTransformPoints affData
    in
        data
            |> aff
            |> List.map point2String
            |> String.join " "


{-| drawPointList graphData "yellow" [(0.0, 0.0), (100.0, 20.0), (200.0, 0.0)]
  produces an SVG representation of the given polygonal path.
-}
drawPointList : GraphData -> String -> Points -> S.Svg msg
drawPointList graphData color data =
    -- polyline [ fill "none", stroke "red", points (data2SVG data) ] []
    polyline [ fill "none", stroke color, points (data2SVG graphData data) ] []


drawPolygon : GraphData -> String -> String -> Float -> Points -> S.Svg msg
drawPolygon graphData strokeColor fillColor opacityValue data =
    polygon [ fill fillColor, stroke strokeColor, opacity (toString opacityValue), points (data2SVG graphData data) ] []


drawRect : GraphData -> String -> String -> Float -> Float -> Float -> Float -> Float -> S.Svg msg
drawRect graphData strokeColor fillColor opacityValue x y width height =
    let
        vertices =
            [ ( x, y ), ( x + width, y ), ( x + width, y + height ), ( x, y + height ) ]
    in
        polygon [ fill fillColor, stroke strokeColor, opacity (toString opacityValue), points (data2SVG graphData vertices) ] []


drawEllipse : GraphData -> String -> String -> Float -> Float -> Float -> Float -> Float -> S.Svg msg
drawEllipse graphData strokeColor fillColor opacityValue x y rx ry =
    let
        center =
            ( x, y )

        affData =
            affineTransformData graphData

        aff =
            affineTransformPoint affData

        ( xx, yy ) =
            aff center

        rxx =
            abs (affData.a) * rx

        ryy =
            abs (affData.c) * ry
    in
        ellipse
            [ fill fillColor
            , stroke strokeColor
            , opacity (toString opacityValue)
            , SA.cx (toString xx)
            , SA.cy (toString yy)
            , SA.rx (toString (rxx))
            , SA.ry (toString (ryy))
            ]
            []


drawCircle : GraphData -> String -> String -> Float -> Float -> Float -> Float -> S.Svg msg
drawCircle graphData strokeColor fillColor opacityValue x y r =
    drawEllipse graphData strokeColor fillColor opacityValue x y r r


drawLine : GraphData -> String -> Float -> Float -> Float -> Float -> S.Svg msg
drawLine graphData color x1 y1 x2 y2 =
    drawPointList graphData color [ ( x1, y1 ), ( x2, y2 ) ]


{-| drawTimeSeries "yellow" graphData [1.0, 1.2, 3.1, 2.2, ..)]
  produces an SVG representation the polgonal path
  [(0, 1.0), (1, 1.2), (2, 3.1), (3, 2.2), ..)]
-}
drawTimeSeries : GraphData -> String -> List Float -> S.Svg msg
drawTimeSeries graphData color data =
    data |> timeSeries |> drawPointList graphData color


{-| drawIntegerTimeSeries "yellow" graphData [1, 2, 3, 2, ..)]
  produces an SVG representation the polgonal path
  [(0, 1), (1, 2), (2, 3), (3, 2), ..)]
-}
drawIntegerTimeSeries : GraphData -> String -> List Int -> S.Svg msg
drawIntegerTimeSeries graphData color data =
    data |> List.map toFloat |> timeSeries |> drawPointList graphData color
