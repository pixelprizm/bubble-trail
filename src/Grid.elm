module Grid exposing (..)

import Debug
import Window
import Mouse


type Shape
    = Square
    | Hex HexShape


type HexShape
    = PointyTop
    | FlatTop


type alias GridConfig =
    { diameter : Float
    , shape : Shape
    }


type alias GridCoords =
    { grid_x : Int
    , grid_y : Int
    }


type alias SvgCoords =
    { svg_x : Float
    , svg_y : Float
    }


type alias RealCoords =
    { real_x : Float
    , real_y : Float
    }


mouseToRealCoordinates : Window.Size -> Mouse.Position -> RealCoords
mouseToRealCoordinates { width, height } { x, y } =
    RealCoords
        (toFloat x - (toFloat width / 2))
        -(toFloat y - (toFloat height / 2))


realToSvgCoordinates : RealCoords -> SvgCoords
realToSvgCoordinates { real_x, real_y } =
    SvgCoords real_x -real_y


getGridCoords : GridConfig -> RealCoords -> GridCoords
getGridCoords { diameter, shape } source =
    case shape of
        Square ->
            GridCoords
                (round (source.real_x / diameter))
                (round (source.real_y / diameter))

        Hex hexShape ->
            let
                realToGrid_pointyTop : Float -> RealCoords -> GridCoords
                realToGrid_pointyTop diameter { real_x, real_y } =
                    let
                        miniWidth =
                            diameter / 2

                        miniHeight =
                            miniWidth / (sqrt 3)

                        miniX : Float
                        miniX =
                            real_x / miniWidth

                        miniY : Float
                        miniY =
                            real_y / miniHeight

                        miniCol : Int
                        miniCol =
                            floor miniX

                        miniRow : Int
                        miniRow =
                            floor miniY

                        miniRowColToGrid : Int -> Int -> GridCoords
                        miniRowColToGrid mCol mRow =
                            GridCoords
                                (mCol // 2)
                                (mRow // 3)

                        miniRowMod6 =
                            miniRow % 6

                        gridCoordsEasyRow : Int -> Int -> GridCoords
                        gridCoordsEasyRow mC mR =
                            -- This function is defined if mR is an easy row (one of the
                            --   mini-rows without zig zags)
                            let
                                gridY =
                                    (toFloat mR + 1) / 3 |> floor

                                gridX =
                                    toFloat (mC + (gridY + 1) % 2) / 2 |> floor
                            in
                                GridCoords
                                    gridX
                                    gridY
                    in
                        if miniRowMod6 == 1 || miniRowMod6 == 4 then
                            -- This is in a hard row (the mini-row with the zig zags).
                            -- Change the data into an easy row.
                            let
                                offsetX : Float
                                offsetX =
                                    miniX - toFloat miniCol

                                offsetY : Float
                                offsetY =
                                    miniY - toFloat miniRow

                                old =
                                    if (miniCol + miniRow // 3) % 2 == 0 then
                                        if offsetY < 1 - offsetX then
                                            miniRowColToGrid
                                                (miniCol)
                                                (miniRow - 1)
                                        else
                                            miniRowColToGrid
                                                (miniCol + 1)
                                                (miniRow + 2)
                                    else if offsetY < offsetX then
                                        miniRowColToGrid
                                            (miniCol + 1)
                                            (miniRow - 1)
                                    else
                                        miniRowColToGrid
                                            (miniCol)
                                            (miniRow + 2)

                                -- 1 if the zigzag is like y=(1-x)+miniRow, 0 if the zigzag is like y=(x)+miniRow
                                negativeSlopeIndicator : Int
                                negativeSlopeIndicator =
                                    (miniCol + miniRow) % 2

                                -- This is like y=1-x or y=x
                                zigzagLineValue : Float
                                zigzagLineValue =
                                    toFloat negativeSlopeIndicator
                                        + (toFloat (1 - 2 * negativeSlopeIndicator) * offsetX)

                                -- This is 0 if y>=x, -1 if y<x
                                decreaseRowIndicator : Int
                                decreaseRowIndicator =
                                    floor (offsetY - zigzagLineValue)

                                adjustedMiniRow : Int
                                adjustedMiniRow =
                                    miniRow
                                        + (1 + 2 * decreaseRowIndicator)
                            in
                                gridCoordsEasyRow
                                    miniCol
                                    adjustedMiniRow
                        else
                            gridCoordsEasyRow miniCol miniRow
            in
                (case hexShape of
                    PointyTop ->
                        source

                    FlatTop ->
                        RealCoords source.real_y source.real_x
                )
                    |> realToGrid_pointyTop diameter


getCenter : GridConfig -> GridCoords -> RealCoords
getCenter { diameter, shape } { grid_x, grid_y } =
    case shape of
        Square ->
            RealCoords
                (diameter * toFloat grid_x)
                (diameter * toFloat grid_y)

        Hex hexShape ->
            let
                gridHeightInPx : Float
                gridHeightInPx =
                    (diameter / 2) * sqrt 3

                center_pointyTop =
                    RealCoords
                        ((diameter * toFloat grid_x) + (toFloat (grid_y % 2) * diameter / 2))
                        (gridHeightInPx * toFloat grid_y)
            in
                case hexShape of
                    PointyTop ->
                        center_pointyTop

                    FlatTop ->
                        RealCoords center_pointyTop.real_y center_pointyTop.real_x
