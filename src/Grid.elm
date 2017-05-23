module Grid exposing (..)

import Debug


type Shape
    = Square
    | HexPointyTop
    | HexFlatTop


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
pixelToGrid { diameter, shape } { x, y } =
    case shape of
        Square ->
            GridCoords
                (round (x / diameter))
                (round (y / diameter))

        HexPointyTop ->
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
                    Debug.log "adjustedMiniRow" <|
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
                        Debug.log "coords" <|
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

        HexFlatTop ->
            GridCoords 0 0


gridToCenterPixel : GridConfig -> GridCoords -> PixelCoords
gridToCenterPixel { diameter, shape } { gX, gY } =
    case shape of
        Square ->
            PixelCoords
                (diameter * toFloat gX)
                (diameter * toFloat gY)

        HexPointyTop ->
            let
                gridHeightInPx : Float
                gridHeightInPx =
                    (diameter / 2) * sqrt 3
            in
                PixelCoords
                    ((diameter * toFloat gX) + (toFloat (gY % 2) * diameter / 2))
                    (gridHeightInPx * toFloat gY)

        HexFlatTop ->
            PixelCoords 0 0
