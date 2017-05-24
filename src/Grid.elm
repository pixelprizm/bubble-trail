module Grid exposing (..)

import Debug


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
    { gX : Int
    , gY : Int
    }


type alias PixelCoords =
    { x : Float
    , y : Float
    }


pixelToGrid : GridConfig -> PixelCoords -> GridCoords
pixelToGrid { diameter, shape } pixel =
    case shape of
        Square ->
            GridCoords
                (round (pixel.x / diameter))
                (round (pixel.y / diameter))

        Hex PointyTop ->
            pixelToGrid_HexPointyTop diameter pixel.x pixel.y

        Hex FlatTop ->
            let
                pointyTopGridCoords =
                    pixelToGrid_HexPointyTop diameter pixel.y pixel.x
            in
                GridCoords pointyTopGridCoords.gX pointyTopGridCoords.gY


gridToCenterPixel : GridConfig -> GridCoords -> PixelCoords
gridToCenterPixel { diameter, shape } { gX, gY } =
    let
        gridHeightInPx : Float
        gridHeightInPx =
            (diameter / 2) * sqrt 3

        centerPixel_HexPointyTop =
            PixelCoords
                ((diameter * toFloat gX) + (toFloat (gY % 2) * diameter / 2))
                (gridHeightInPx * toFloat gY)
    in
        case shape of
            Square ->
                PixelCoords
                    (diameter * toFloat gX)
                    (diameter * toFloat gY)

            Hex PointyTop ->
                centerPixel_HexPointyTop

            Hex FlatTop ->
                PixelCoords centerPixel_HexPointyTop.y centerPixel_HexPointyTop.x


pixelToGrid_HexPointyTop : Float -> Float -> Float -> GridCoords
pixelToGrid_HexPointyTop diameter x y =
    let
        miniWidth =
            diameter / 2

        miniHeight =
            miniWidth / (sqrt 3)

        miniX : Float
        miniX =
            x / miniWidth

        miniY : Float
        miniY =
            y / miniHeight

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
