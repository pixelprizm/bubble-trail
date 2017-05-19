module Grid exposing (..)


type GridShape
    = Square
    | HexPointyTop
    | HexFlatTop


type alias GridConfig =
    { diameter : Float
    , shape : GridShape
    }


type alias GridCoords =
    { q : Int
    , r : Int
    }


type alias PixelCoords =
    { x : Float
    , y : Float
    }


pixelToGrid : GridConfig -> PixelCoords -> GridCoords
pixelToGrid { diameter, shape } { x, y } =
    case shape of
        Square ->
            PixelCoords
                (diameter * round (x / diameter))
                (diameter * round (y / diameter))

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
                    miniX - miniCol

                offsetY : Float
                offsetY =
                    miniY - miniRow

                miniRowColToGrid : Int -> Int -> GridCoords
                miniRowColToGrid q r =
                    GridCoords
                        (q // 2)
                        (r // 3)

                miniRow6 = miniRow % 6

                bigRow = miniRow // 3

            in
                if miniRow6 == 1 || miniRow == 4
                then
                    if (miniCol + bigRow) % 2 == 0
                    then
                        if offsetY < 1 - offsetX
                        then
                            miniRowColToGrid
                                (miniCol)
                                (miniRow - 1)
                        else
                            miniRowColToGrid
                                (miniCol + 1)
                                (miniRow + 2)
                    else
                        if offsetY < offsetX
                        then
                            miniRowColToGrid
                                (miniCol + 1)
                                (miniRow - 1)
                        else
                            miniRowColToGrid
                                (miniCol)
                                (miniRow + 2)
                else
                    GridCoords
                        (if bigRow % 2 == 0 then
                            round (miniX / 2)
                         else
                            floor (miniX / 2)
                        )
                        (round (miniY / 3))




                --(round <|
                --    miniX
                --        / 2
                --)
                --    (if (miniRow) % 3 == 1 then
                --        (if (miniCol) % 2 == 0 then
                --            ()
                --            --(if miniX - (miniCol) )
                --         else
                --            ()
                --        )
                --     else
                --        round (miniY / 3)
                --    )
            in
                PixelCoords
                    ()

        HexFlatTop ->
            PixelCoords 0 0
