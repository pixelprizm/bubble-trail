module SizeConfig
    exposing
        ( fromSegments
        , SizeSegment
        , SizeSegmentCurve(..)
        , SizeConfig
        , sizePeriod
        , getRadiuses
        )

-- Others'

import Array


type SizeConfig
    = SizeConfig (List SizeSegment)


fromSegments : List SizeSegment -> SizeConfig
fromSegments l =
    SizeConfig l


type alias SizeSegment =
    { length : Int
    , curve : SizeSegmentCurve
    }


type SizeSegmentCurve
    = Linear
        { startRadius : Float
        , endRadius : Float
        , startInclusive : Bool
        , endInclusive : Bool
        }
    | Inverse { initialRadius : Float }
    | Constant Float


sizePeriod : SizeConfig -> Int
sizePeriod (SizeConfig segments) =
    segments
        |> List.map (.length)
        |> List.sum


getRadiuses : SizeConfig -> Array.Array Float
getRadiuses (SizeConfig segments) =
    let
        getArrayForSegment : SizeSegment -> Array.Array Float
        getArrayForSegment segment =
            Array.initialize segment.length
                (\indexWithinSegment ->
                    case segment.curve of
                        Linear { startRadius, endRadius, startInclusive, endInclusive } ->
                            let
                                lengthForInterpolation : Int
                                lengthForInterpolation =
                                    case ( startInclusive, endInclusive ) of
                                        ( False, False ) ->
                                            segment.length + 1

                                        ( False, True ) ->
                                            segment.length

                                        ( True, False ) ->
                                            segment.length

                                        ( True, True ) ->
                                            segment.length - 1

                                indexForInterpolation : Int
                                indexForInterpolation =
                                    if startInclusive then
                                        indexWithinSegment
                                    else
                                        indexWithinSegment + 1

                                interpolationRatio : Float
                                interpolationRatio =
                                    toFloat indexForInterpolation / toFloat lengthForInterpolation
                            in
                                startRadius * (1 - interpolationRatio) + endRadius * interpolationRatio

                        Inverse { initialRadius } ->
                            initialRadius / toFloat (indexWithinSegment + 1)

                        Constant radius ->
                            radius
                )
    in
        segments
            |> List.map getArrayForSegment
            |> List.foldr Array.append Array.empty



{-
   i =
       (index % (chosenConfig.sizeConfig |> SizeConfig.sizePeriod))

   i1 =
       i + 1

   i1WithGaps =
       (index % ((chosenConfig.sizeConfig |> SizeConfig.sizePeriod) * 2))


   outerRadius =
       --case i of
       --    0 ->
       --        chosenConfig.spotRadius
       --    _ ->
       --        chosenConfig.spotRadius / 2
       --chosenConfig.spotRadius * toFloat index * 0.1
       --chosenConfig.spotRadius
       --chosenConfig.spotRadius * toFloat (chosenConfig.sizePeriod - i1WithGaps) / toFloat chosenConfig.sizePeriod
       --chosenConfig.spotRadius / toFloat i1
       --chosenConfig.spotRadius
       chosenConfig.spotRadius * toFloat (chosenConfig.sizePeriod - i) / toFloat chosenConfig.sizePeriod
-}
