module App exposing (..)

-- Others'

import Array
import Char
import Debug
import Html as H
import Html.Attributes as H
import Keyboard.Extra as KE
import Mouse
import Svg as S
import Svg.Attributes as SA
import Task
import Window


-- Mine

import CreativeCommonsLicense
import Grid
import Config exposing (Config)
import SizeConfig


chosenConfig : Config
chosenConfig =
    --Config.justGrowHex Grid.PointyTop
    --Config.justGrowSquare
    --Config.growShrinkSquare
    --Config.growShrinkHex Grid.FlatTop
    --Config.rainbowPulseSquare
    Config.rainbowPulseHex Grid.FlatTop


type alias Spot =
    { gridCoords : Grid.GridCoords
    , index : Int
    }


type alias Model =
    { spots : List Spot
    , windowSize : Window.Size
    , pressedKeys : List KE.Key
    , lockLineStart : Maybe Grid.GridCoords
    }


init : ( Model, Cmd Msg )
init =
    { spots = []
    , windowSize = Window.Size 0 0
    , pressedKeys = []
    , lockLineStart = Nothing
    }
        ! [ Task.perform WindowResize Window.size
          ]


type Msg
    = MouseMove Mouse.Position
    | KeyMsg KE.Msg
    | WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        MouseMove mousePosition ->
            if model.pressedKeys |> List.member KE.Space then
                model ! []
            else
                let
                    mouse : Grid.RealCoords
                    mouse =
                        mousePosition |> Grid.mouseToRealCoordinates model.windowSize

                    nearestGridToMouse : Grid.GridCoords
                    nearestGridToMouse =
                        mouse |> Grid.getGridCoords (chosenConfig |> Config.getGridConfig)

                    possibleNewSpotCoords =
                        case model.lockLineStart of
                            Nothing ->
                                nearestGridToMouse

                            Just lockLineStart ->
                                let
                                    lockLineStartCenter =
                                        lockLineStart |> Grid.getCenter (chosenConfig |> Config.getGridConfig)

                                    lineX : Float
                                    lineX =
                                        mouse.real_x - lockLineStartCenter.real_x

                                    lineY : Float
                                    lineY =
                                        mouse.real_y - lockLineStartCenter.real_y

                                    ( r, theta ) =
                                        Debug.log "r, theta" <|
                                            toPolar ( lineX, lineY )

                                    thetaRoundingIncrement =
                                        case chosenConfig.shape of
                                            Grid.Square ->
                                                turns (1 / 4)

                                            Grid.Hex _ ->
                                                turns (1 / 6)

                                    roundOrFloor =
                                        if chosenConfig.shape == Grid.Hex Grid.FlatTop then
                                            floor
                                        else
                                            round

                                    offset =
                                        if chosenConfig.shape == Grid.Hex Grid.FlatTop then
                                            turns (1 / 12)
                                        else
                                            0

                                    snappedTheta : Float
                                    snappedTheta =
                                        theta
                                            / thetaRoundingIncrement
                                            |> roundOrFloor
                                            |> toFloat
                                            |> (*) thetaRoundingIncrement
                                            |> (+) offset

                                    test =
                                        Debug.log "snappedTheta" snappedTheta

                                    ( snappedX, snappedY ) =
                                        fromPolar ( r, snappedTheta )
                                in
                                    Grid.getGridCoords (chosenConfig |> Config.getGridConfig) <|
                                        Grid.RealCoords
                                            (lockLineStartCenter.real_x + snappedX)
                                            (lockLineStartCenter.real_y + snappedY)
                in
                    case List.head model.spots of
                        Nothing ->
                            { model | spots = [ Spot possibleNewSpotCoords 0 ] } ! []

                        Just spot ->
                            if spot.gridCoords == possibleNewSpotCoords then
                                model ! []
                            else
                                { model
                                    | spots = (Spot possibleNewSpotCoords (spot.index + 1)) :: model.spots
                                }
                                    ! []

        KeyMsg keyMsg ->
            let
                ( pressedKeys, possibleKeyChange ) =
                    KE.updateWithKeyChange keyMsg model.pressedKeys

                modelWithPressedKeysUpdated =
                    { model | pressedKeys = pressedKeys }

                modelTakingIntoAccountBackspace =
                    if pressedKeys |> List.member KE.BackSpace then
                        { modelWithPressedKeysUpdated
                            | spots = modelWithPressedKeysUpdated.spots |> List.drop 1
                        }
                    else
                        modelWithPressedKeysUpdated
            in
                case possibleKeyChange of
                    Nothing ->
                        modelTakingIntoAccountBackspace ! []

                    Just (KE.KeyDown KE.Shift) ->
                        let
                            test =
                                Debug.log "shift down" ""

                            lockLineStart : Grid.GridCoords
                            lockLineStart =
                                case List.head model.spots of
                                    Nothing ->
                                        Grid.GridCoords 0 0

                                    Just spot ->
                                        spot.gridCoords
                        in
                            { modelTakingIntoAccountBackspace | lockLineStart = Just lockLineStart } ! []

                    Just (KE.KeyDown key) ->
                        let
                            test =
                                Debug.log "key down" key
                        in
                            modelTakingIntoAccountBackspace ! []

                    Just (KE.KeyUp KE.Shift) ->
                        let
                            test =
                                Debug.log "shift up" ""
                        in
                            { modelTakingIntoAccountBackspace | lockLineStart = Nothing } ! []

                    Just (KE.KeyUp key) ->
                        let
                            test =
                                Debug.log "key up" key
                        in
                            modelTakingIntoAccountBackspace ! []

        WindowResize size ->
            { model | windowSize = size } ! []
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResize
        , Mouse.moves MouseMove
        , Sub.map KeyMsg KE.subscriptions
        ]


view : Model -> H.Html Msg
view model =
    H.div [ H.id "app" ]
        [ H.div [ H.id "overlay" ]
            [ CreativeCommonsLicense.view [ H.class "license" ]
            ]
        , H.div [ H.class "svgContainer" ]
            [ S.svg
                [ SA.id "graphics"
                , SA.viewBox <|
                    String.concat
                        [ toFloat -model.windowSize.width / 2 |> toString
                        , " "
                        , toFloat -model.windowSize.height / 2 |> toString
                        , " "
                        , model.windowSize.width |> toString
                        , " "
                        , model.windowSize.height |> toString
                        ]
                , SA.width (model.windowSize.width |> toString)
                , SA.height (model.windowSize.height |> toString)
                ]
                ((List.indexedMap
                    (viewSpot <| SizeConfig.getRadiuses chosenConfig.sizeConfig)
                    (model.spots |> List.take chosenConfig.spotCount)
                    |> (if chosenConfig.newInBack then
                            identity
                        else
                            -- Reverse so that new spots show up over old spots:
                            List.reverse
                       )
                 )
                 --++ ((List.range -100 100)
                 --        |> List.map
                 --            (\i ->
                 --                S.g []
                 --                    -- Vertical line
                 --                    [ S.line
                 --                        [ SA.x1 (toString (toFloat i * chosenConfig.diameter / 2))
                 --                        , SA.y1 "-4000"
                 --                        , SA.x2 (toString (toFloat i * chosenConfig.diameter / 2))
                 --                        , SA.y2 "4000"
                 --                        , SA.stroke "gray"
                 --                        ]
                 --                        []
                 --                    -- Horizontal line
                 --                    , S.line
                 --                        [ SA.x1 "-4000"
                 --                        , SA.y1 (toString (toFloat i * chosenConfig.diameter / 2 / sqrt 3))
                 --                        , SA.x2 "4000"
                 --                        , SA.y2 (toString (toFloat i * chosenConfig.diameter / 2 / sqrt 3))
                 --                        , SA.stroke "gray"
                 --                        ]
                 --                        []
                 --                    ]
                 --            )
                 --   )
                )
            ]
        ]


viewSpot : Array.Array Float -> Int -> Spot -> S.Svg Msg
viewSpot radiuses index spot =
    S.g []
        (let
            --useInner =
            --    False
            --innerRadius =
            --    --outerRadius * 0.8
            --    --chosenConfig.spotRadius / toFloat i1
            --    0.8 * chosenConfig.spotRadius * toFloat (chosenConfig.sizePeriod - i) / toFloat chosenConfig.sizePeriod
            { svg_x, svg_y } =
                Grid.getCenter (chosenConfig |> Config.getGridConfig) spot.gridCoords
                    |> Grid.realToSvgCoordinates

            indexForColorRatio =
                if chosenConfig.colorsMove then
                    index
                else
                    spot.index

            colorRatio =
                toFloat indexForColorRatio / toFloat chosenConfig.colorPeriod

            hue =
                colorRatio
                    * (if chosenConfig.naturalColors then
                        240
                       else
                        360
                      )

            sizePeriod : Int
            sizePeriod =
                Array.length radiuses

            radius : Float
            radius =
                case radiuses |> Array.get (index % sizePeriod) of
                    Nothing ->
                        0

                    Just r ->
                        r

            outerHsl =
                --{ h = 0
                { h = hue

                --, s = 0
                , s = 100

                --, l = 70
                --, l = 0
                , l = 50
                }

            --innerHsl =
            --    { h = hue
            --    --{ h = hue
            --    --, s = 0
            --    , s = 100
            --    --, l = 0
            --    --, l = 70
            --    , l = 50
            --    }
            hslToString { h, s, l } =
                "hsl(" ++ toString h ++ "," ++ toString s ++ "%," ++ toString l ++ "%)"
         in
            ([ S.circle
                [ SA.cx (svg_x |> toString)
                , SA.cy (svg_y |> toString)
                , SA.r (radius |> toString)
                , SA.fill (outerHsl |> hslToString)
                ]
                []
             ]
             --++ if useInner then
             --    [ S.circle
             --        [ SA.cx (x |> toString)
             --        , SA.cy (y |> toString)
             --        , SA.r (innerRadius |> toString)
             --        , SA.fill (innerHsl |> hslToString)
             --        ]
             --        []
             --    ]
             --   else
             --    []
            )
        )
