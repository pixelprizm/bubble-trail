module Grid exposing (..)

import Debug


type GridShape
    = Square
    | HexPointyTop
    | HexFlatTop


type alias GridConfig =
    { diameter : Float
    , shape : GridShape
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
                (floor (x / diameter))
                (floor (y / diameter))

        HexPointyTop ->
            let
                miniW =
                    diameter / 2

                miniH =
                    miniW / (sqrt 3)

                miniX : Float
                miniX =
                    x / miniW

                miniY : Float
                miniY =
                    y / miniH

                miniCol : Int
                miniCol =
                    floor miniX

                miniRow : Int
                miniRow =
                    floor miniY

                offsetX : Float
                offsetX =
                    miniX - toFloat miniCol

                offsetY : Float
                offsetY =
                    miniY - toFloat miniRow

                miniRowColToGrid : Int -> Int -> GridCoords
                miniRowColToGrid mCol mRow =
                    GridCoords
                        (mCol // 2)
                        (mRow // 3)

                miniRowMod6 =
                    miniRow % 6
            in
                if miniRowMod6 == 1 || miniRowMod6 == 4 then
                    -- This is in a hard row (the mini-row with the zig zags)
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
                else
                    -- This is an easy row (one of the mini-rows without zig zags)
                    --Debug.log "easy row, calculatedCoords"
                    let
                        gridY =
                            (toFloat miniRow + 1) / 3 |> floor

                        gridX =
                            toFloat (miniCol + (gridY + 1) % 2) / 2 |> floor
                    in
                        GridCoords
                            gridX
                            gridY

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
