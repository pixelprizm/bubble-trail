module SizeConfig
    exposing
        ( fromSegments
        , SizeSegment
        , SizeSegmentCurve(..)
        , SizeConfig
        , sizePeriod
        , getRadiuses
        )


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
    = Linear { startRadius : Float, endRadius : Float }
    | Inverse { initialRadius : Float }
    | Constant Float


sizePeriod : SizeConfig -> Int
sizePeriod (SizeConfig segments) =
    segments
        |> List.map (.length)
        |> List.sum


getRadiuses : SizeConfig -> List Float
getRadiuses (SizeConfig segments) =
    segments
        |> List.concatMap
            (\segment ->
                List.range 0 (segment.length - 1)
                    |> List.map
                        (\indexWithinSegment ->
                            case segment.curve of
                                Linear { startRadius, endRadius } ->
                                    let
                                        interpolationRatio : Float
                                        interpolationRatio =
                                            toFloat (segment.length - indexWithinSegment) / toFloat segment.length
                                    in
                                        startRadius * interpolationRatio + endRadius * interpolationRatio

                                Inverse { initialRadius } ->
                                    initialRadius / toFloat (indexWithinSegment + 1)

                                Constant radius ->
                                    radius
                        )
            )



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
