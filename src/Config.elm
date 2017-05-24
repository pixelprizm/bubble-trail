module Config exposing (..)

import SizeConfig
import Grid


type alias Config =
    { diameter : Float
    , shape : Grid.Shape
    , sizeConfig : SizeConfig.SizeConfig
    , colorPeriod : Int
    , spotCount : Int
    , naturalColors : Bool
    }


getGridConfig : Config -> Grid.GridConfig
getGridConfig c =
    Grid.GridConfig
        c.diameter
        c.shape



--spotRadius: 0.9 * (chosenConfig.diameter / 2)
--colorPeriod: sizePeriod * 2
--sizePeriod: 120
--spotCount: colorPeriod
--spotCount: sizePeriod
--spotCount: * 6; * 3


rainbowPulseSquare : Config
rainbowPulseSquare =
    rainbowPulse Grid.Square 20 1.6


rainbowPulseHex : Config
rainbowPulseHex =
    rainbowPulse Grid.HexPointyTop 24 1.4


rainbowPulse : Grid.Shape -> Float -> Float -> Config
rainbowPulse shape diameter startSizeFactor =
    let
        sizePeriod =
            12

        colorPeriod =
            sizePeriod - 1
    in
        { diameter = diameter
        , shape = shape
        , colorPeriod = colorPeriod
        , sizeConfig =
            SizeConfig.fromSegments
                [ { length = sizePeriod
                  , curve =
                        SizeConfig.Linear
                            { startRadius = diameter / 2 * startSizeFactor
                            , endRadius = 0
                            }
                  }
                ]
        , spotCount = colorPeriod * sizePeriod
        , naturalColors = False
        }
