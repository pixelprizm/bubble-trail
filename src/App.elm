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


type alias Spot =
    { index : Int
    , gridCoords : Grid.GridCoords
    }


type alias Model =
    { config : Config
    , spots : List Spot
    , windowSize : Window.Size
    , pressedKeys : List KE.Key
    , lockLine :
        Maybe
            { start : Grid.GridCoords
            , end : Maybe Grid.RealCoords
            }
    }


getNewSpotIndex : Model -> Int
getNewSpotIndex model =
    case List.head model.spots of
        Nothing ->
            0

        Just spot ->
            spot.index + 1


getProvisionalSpots : Model -> List Spot
getProvisionalSpots model =
    case model.lockLine of
        Nothing ->
            []

        Just { start, end } ->
            case end of
                Nothing ->
                    []

                Just end ->
                    Grid.getSnappedLineCoords
                        (model.config |> Config.getGridConfig)
                        start
                        end
                        |> List.indexedMap
                            (\i coords -> Spot (i + getNewSpotIndex model) coords)


init : ( Model, Cmd Msg )
init =
    { config =
        --Config.justGrowHex Grid.PointyTop
        --Config.justGrowSquare
        --Config.growShrinkSquare
        --Config.growShrinkHex Grid.FlatTop
        --Config.rainbowPulseHex Grid.PointyTop
        Config.rainbowPulseSquare
    , spots = []
    , windowSize = Window.Size 0 0
    , pressedKeys = []
    , lockLine = Nothing
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
                        mouse |> Grid.getGridCoords (model.config |> Config.getGridConfig)
                in
                    case model.lockLine of
                        Nothing ->
                            let
                                newSpotIndex : Int
                                newSpotIndex =
                                    getNewSpotIndex model
                            in
                                case List.head model.spots of
                                    Nothing ->
                                        { model | spots = [ Spot newSpotIndex nearestGridToMouse ] } ! []

                                    Just spot ->
                                        if spot.gridCoords == nearestGridToMouse then
                                            model ! []
                                        else
                                            { model
                                                | spots = (Spot newSpotIndex nearestGridToMouse) :: model.spots
                                            }
                                                ! []

                        Just lockLineDefinition ->
                            { model
                                | lockLine =
                                    Just
                                        { lockLineDefinition
                                            | end = Just mouse
                                        }
                            }
                                ! []

        KeyMsg keyMsg ->
            let
                ( pressedKeys, possibleKeyChange ) =
                    KE.updateWithKeyChange keyMsg model.pressedKeys

                modelWithPressedKeysUpdated =
                    { model | pressedKeys = pressedKeys }

                modelTakingIntoAccountDeleting =
                    if (pressedKeys |> List.member KE.Tab) || (pressedKeys |> List.member KE.BackSpace) then
                        { modelWithPressedKeysUpdated
                            | spots = modelWithPressedKeysUpdated.spots |> List.drop 1
                        }
                    else
                        modelWithPressedKeysUpdated
            in
                case possibleKeyChange of
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
                            { modelTakingIntoAccountDeleting | lockLine = Just { start = lockLineStart, end = Nothing } } ! []

                    Just (KE.KeyDown KE.CharL) ->
                        let
                            oldConfig =
                                model.config
                        in
                            { modelTakingIntoAccountDeleting
                                | config =
                                    { oldConfig
                                        | limitSpots = (not model.config.limitSpots)
                                    }
                            }
                                ! []

                    Just (KE.KeyDown KE.Delete) ->
                        { modelTakingIntoAccountDeleting | spots = [] }
                            ! []

                    Just (KE.KeyUp KE.Shift) ->
                        let
                            test =
                                Debug.log "shift up" ""
                        in
                            { modelTakingIntoAccountDeleting
                                | lockLine = Nothing
                                , spots = List.append (getProvisionalSpots model) model.spots
                            }
                                ! []

                    _ ->
                        modelTakingIntoAccountDeleting ! []

        WindowResize size ->
            { model | windowSize = size } ! []
    )
        |> limitSpots


{-| Limit spot count, leaving extra for the purpose of undo
-}
limitSpots : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
limitSpots ( model, c ) =
    ( { model
        | spots = model.spots |> List.take (model.config.spotCount * 2)
      }
    , c
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
                    (viewSpot model.config)
                    (List.append (getProvisionalSpots model) model.spots
                        |> if model.config.limitSpots then
                            List.take model.config.spotCount
                           else
                            identity
                    )
                    |> (if model.config.newInBack then
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
                 --                        [ SA.x1 (toString (toFloat i * model.config.diameter / 2))
                 --                        , SA.y1 "-4000"
                 --                        , SA.x2 (toString (toFloat i * model.config.diameter / 2))
                 --                        , SA.y2 "4000"
                 --                        , SA.stroke "gray"
                 --                        ]
                 --                        []
                 --                    -- Horizontal line
                 --                    , S.line
                 --                        [ SA.x1 "-4000"
                 --                        , SA.y1 (toString (toFloat i * model.config.diameter / 2 / sqrt 3))
                 --                        , SA.x2 "4000"
                 --                        , SA.y2 (toString (toFloat i * model.config.diameter / 2 / sqrt 3))
                 --                        , SA.stroke "gray"
                 --                        ]
                 --                        []
                 --                    ]
                 --            )
                 --   )
                )
            ]
        ]


viewSpot : Config -> Int -> Spot -> S.Svg Msg
viewSpot config index spot =
    S.g []
        (let
            --useInner =
            --    False
            --innerRadius =
            --    --outerRadius * 0.8
            --    --config.spotRadius / toFloat i1
            --    0.8 * config.spotRadius * toFloat (config.sizePeriod - i) / toFloat config.sizePeriod
            { svg_x, svg_y } =
                Grid.getCenter (config |> Config.getGridConfig) spot.gridCoords
                    |> Grid.realToSvgCoordinates

            indexForColorRatio =
                if config.colorsMove then
                    index
                else
                    spot.index

            colorRatio =
                toFloat indexForColorRatio / toFloat config.colorPeriod

            hue =
                colorRatio
                    * (if config.naturalColors then
                        240
                       else
                        360
                      )

            radiuses : Array.Array Float
            radiuses =
                Config.getRadiuses config

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
