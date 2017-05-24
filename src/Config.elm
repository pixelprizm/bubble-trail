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



--spotRadius: 0.9 * (diameter / 2)
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


growShrinkSquare : Config
growShrinkSquare =
    growShrink Grid.Square 20 1.6


growShrinkHex : Config
growShrinkHex =
    growShrink Grid.HexPointyTop 24 1.4


rainbowPulse : Grid.Shape -> Float -> Float -> Config
rainbowPulse shape diameter bigRadiusFactor =
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
                            { startRadius = (diameter / 2) * bigRadiusFactor
                            , endRadius = 0
                            , startInclusive = True
                            , endInclusive = True
                            }
                  }
                ]
        , spotCount = colorPeriod * sizePeriod
        , naturalColors = False
        }


growShrink : Grid.Shape -> Float -> Float -> Config
growShrink shape diameter bigRadiusFactor =
    let
        growLength : Int
        growLength =
            10

        shrinkLength : Int
        shrinkLength =
            growLength

        bigRadius : Float
        bigRadius =
            (diameter / 2) * bigRadiusFactor

        smallRadius : Float
        smallRadius =
            bigRadius / toFloat growLength / 2

        sizeConfig =
            SizeConfig.fromSegments
                [ { length = growLength
                  , curve =
                        SizeConfig.Linear
                            { startRadius = smallRadius
                            , endRadius = bigRadius
                            , startInclusive = True
                            , endInclusive = False
                            }
                  }
                , { length = shrinkLength
                  , curve =
                        SizeConfig.Linear
                            { startRadius = bigRadius
                            , endRadius = smallRadius
                            , startInclusive = True
                            , endInclusive = False
                            }
                  }
                ]

        sizePeriod =
            SizeConfig.sizePeriod sizeConfig

        colorPeriod : Int
        colorPeriod =
            sizePeriod // 2 - 1
    in
        { diameter = diameter
        , shape = shape
        , colorPeriod = colorPeriod
        , sizeConfig = sizeConfig
        , spotCount = sizePeriod * colorPeriod
        , naturalColors = False
        }
