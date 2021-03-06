module Main exposing (..)

import Html exposing (Html)
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Circle2d as Circle2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Svg as Svg
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Vector2d as Vector2d
import Svg as Svg exposing (Svg)
import Svg.Attributes as Attributes


main : Html msg
main =
    let
        --Raw geometry
        angles =
            List.map (\index -> degrees (6 * toFloat index)) (List.range 0 15)

        points =
            angles
                |> List.map
                    (\angle -> Point2d.fromPolarCoordinates ( 300, angle ))

        lineSegments =
            points
                |> List.map
                    (\point ->
                        LineSegment2d.fromEndpoints ( Point2d.origin, point )
                    )

        firstTriangle =
            Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 300, -10 )
                , Point2d.fromCoordinates ( 320, 0 )
                , Point2d.fromCoordinates ( 300, 10 )
                )

        rotatedTriangle angle =
            Triangle2d.rotateAround Point2d.origin angle firstTriangle

        triangles =
            List.map rotatedTriangle angles

        -- Drawing functions
        point2d point =
            Svg.circle2d [ Attributes.fill "cornflowerblue" ]
                (Circle2d.with { centerPoint = point, radius = 5 })

        lineSegment2d =
            Svg.lineSegment2d [ Attributes.strokeWidth "0.5" ]

        triangle2d =
            Svg.triangle2d [ Attributes.fill "palegreen" ]

        -- SVG elements
        pointElements =
            List.map point2d points

        lineElements =
            List.map lineSegment2d lineSegments

        triangleElements =
            List.map triangle2d triangles

        allElements =
            List.concat
                [ lineElements
                , triangleElements
                , pointElements
                ]

        transformedGroup =
            Svg.g [] allElements
                |> Svg.translateBy (Vector2d.fromComponents ( 0, 50 ))
                |> Svg.mirrorAcross Axis2d.x
                |> Svg.rotateAround Point2d.origin (degrees 45)

        originElement =
            Svg.circle2d
                [ Attributes.strokeWidth "0.5", Attributes.fill "white" ]
                (Circle2d.with { centerPoint = Point2d.origin, radius = 5 })

        -- Top level group
        defaultAttributes =
            [ Attributes.stroke "black"
            , Attributes.strokeWidth "1"
            ]

        topLevelGroup =
            Svg.g defaultAttributes
                [ transformedGroup
                , originElement
                ]

        topLeftFrame =
            Frame2d.atPoint (Point2d.fromCoordinates ( -400, 400 ))
                |> Frame2d.flipY
    in
    Svg.svg [ Attributes.width "800", Attributes.height "800" ]
        [ Svg.relativeTo topLeftFrame topLevelGroup ]
