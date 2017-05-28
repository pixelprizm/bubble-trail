module App exposing (..)

-- Others'

import Array
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


type alias Spot =
    { index : Int
    , visible : Bool
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
            , unsnappedEnd : Maybe Grid.RealCoords
            }
    , hoverSpotCoords : Maybe Grid.GridCoords
    }


init : ( Model, Cmd Msg )
init =
    { config =
        --Config.justGrowHex Grid.PointyTop
        --Config.justGrowSquare
        --Config.growShrinkSquare
        --Config.growShrinkHex Grid.FlatTop
        --Config.rainbowPulseSquare
        Config.rainbowPulseHex Grid.PointyTop
    , spots = []
    , windowSize = Window.Size 0 0
    , pressedKeys = []
    , lockLine = Nothing
    , hoverSpotCoords = Nothing
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
            let
                mouse : Grid.RealCoords
                mouse =
                    mousePosition |> Grid.mouseToRealCoordinates model.windowSize

                nearestGridToMouse : Grid.GridCoords
                nearestGridToMouse =
                    mouse |> Grid.getGridCoords (model.config |> Config.getGridConfig)
            in
                if model.pressedKeys |> List.member KE.Space then
                    { model
                        | hoverSpotCoords = Just nearestGridToMouse
                    }
                        ! []
                else
                    case model.lockLine of
                        Nothing ->
                            insertNewSpotIfNotRepeat True nearestGridToMouse model
                                ! []

                        Just lockLineDefinition ->
                            { model
                                | lockLine =
                                    Just
                                        { lockLineDefinition
                                            | unsnappedEnd = Just mouse
                                        }
                            }
                                ! []

        KeyMsg keyMsg ->
            let
                ( pressedKeys, possibleKeyChange ) =
                    KE.updateWithKeyChange keyMsg model.pressedKeys

                modelWithPressedKeysUpdated =
                    { model | pressedKeys = pressedKeys }

                modelAfterDeleting =
                    if (pressedKeys |> List.member KE.Tab) || (pressedKeys |> List.member KE.BackSpace) then
                        { modelWithPressedKeysUpdated
                            | spots = modelWithPressedKeysUpdated.spots |> List.drop 1
                        }
                    else
                        modelWithPressedKeysUpdated

                modelAfterInvisibling =
                    if pressedKeys |> List.member KE.CharX then
                        insertNewSpotIfNotRepeat
                            False
                            (Grid.GridCoords 0 0)
                            modelAfterDeleting
                    else
                        modelAfterDeleting
            in
                case possibleKeyChange of
                    Just (KE.KeyDown KE.Shift) ->
                        let
                            lockLineStart : Grid.GridCoords
                            lockLineStart =
                                case List.head model.spots of
                                    Nothing ->
                                        Grid.GridCoords 0 0

                                    Just spot ->
                                        spot.gridCoords
                        in
                            { modelAfterInvisibling
                                | lockLine = Just { start = lockLineStart, unsnappedEnd = Nothing }
                            }
                                ! []

                    Just (KE.KeyDown KE.CharL) ->
                        let
                            oldConfig =
                                model.config
                        in
                            { modelAfterInvisibling
                                | config =
                                    { oldConfig
                                        | limitSpots = (not model.config.limitSpots)
                                    }
                            }
                                ! []

                    Just (KE.KeyDown KE.Delete) ->
                        { modelAfterInvisibling | spots = [] }
                            ! []

                    Just (KE.KeyUp KE.Shift) ->
                        { modelAfterInvisibling
                            | lockLine = Nothing
                            , spots = List.append (getProvisionalSpots model) model.spots
                        }
                            ! []

                    Just (KE.KeyUp KE.Space) ->
                        case modelAfterInvisibling.hoverSpotCoords of
                            Nothing ->
                                model ! []

                            Just coords ->
                                insertNewSpotIfNotRepeat True
                                    coords
                                    { modelAfterInvisibling
                                        | hoverSpotCoords = Nothing
                                    }
                                    ! []

                    _ ->
                        modelAfterInvisibling ! []

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



-- VIEW AND VIEW HELPERS


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
                    ++ [ viewGuidelines model ]
                    ++ [ viewHoverSpot model ]
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
        (if spot.visible then
            let
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
         else
            []
        )


viewGuidelines : Model -> S.Svg Msg
viewGuidelines model =
    S.g [] <|
        case model.lockLine of
            Nothing ->
                []

            Just { start } ->
                viewGuidelinesFrom model start


viewGuidelinesFrom : Model -> Grid.GridCoords -> List (S.Svg Msg)
viewGuidelinesFrom model start =
    let
        lineStartCenter =
            Grid.getCenter
                (model.config |> Config.getGridConfig)
                start
    in
        Grid.getCardinalThetas model.config.shape
            |> List.concatMap
                (\theta ->
                    ( lineStartCenter, 100, theta )
                        |> Grid.getGridLine (model.config |> Config.getGridConfig)
                        |> List.map
                            (\gridCoords ->
                                let
                                    { svg_x, svg_y } =
                                        Grid.getCenter (model.config |> Config.getGridConfig) gridCoords
                                            |> Grid.realToSvgCoordinates

                                    radius =
                                        model.config.diameter * 0.1
                                in
                                    S.circle
                                        [ SA.cx (svg_x |> toString)
                                        , SA.cy (svg_y |> toString)
                                        , SA.r (radius |> toString)
                                        , SA.fill "hsl(0,0%,100%)"
                                        , SA.opacity "0.2"
                                        ]
                                        []
                            )
                )


viewHoverSpot : Model -> S.Svg Msg
viewHoverSpot model =
    S.g [] <|
        case model.hoverSpotCoords of
            Nothing ->
                []

            Just coords ->
                let
                    { svg_x, svg_y } =
                        Grid.getCenter
                            (model.config |> Config.getGridConfig)
                            coords
                            |> Grid.realToSvgCoordinates

                    radius =
                        model.config.diameter / 2
                in
                    [ S.circle
                        [ SA.cx (svg_x |> toString)
                        , SA.cy (svg_y |> toString)
                        , SA.r (radius |> toString)
                        , SA.fill "hsl(0,0%,100%)"
                        , SA.opacity "0.4"
                        ]
                        []
                    ]
                        ++ viewGuidelinesFrom model coords



-- OTHER HELPERS


getNewSpotIndex : Model -> Int
getNewSpotIndex model =
    case List.head model.spots of
        Nothing ->
            0

        Just spot ->
            spot.index + 1


insertNewSpotIfNotRepeat : Bool -> Grid.GridCoords -> Model -> Model
insertNewSpotIfNotRepeat visible coords model =
    let
        newSpotIndex : Int
        newSpotIndex =
            getNewSpotIndex model
    in
        case List.head model.spots of
            Nothing ->
                { model | spots = [ Spot newSpotIndex visible coords ] }

            Just spot ->
                -- Note: if not visible, don't worry if we're making a spot on top of a spot.
                if visible && spot.gridCoords == coords then
                    model
                else
                    { model
                        | spots = (Spot newSpotIndex visible coords) :: model.spots
                    }


getProvisionalSpots : Model -> List Spot
getProvisionalSpots model =
    case model.lockLine of
        Nothing ->
            []

        Just { start, unsnappedEnd } ->
            case unsnappedEnd of
                Nothing ->
                    []

                Just unsnappedEnd ->
                    Grid.snapLine (model.config |> Config.getGridConfig)
                        start
                        unsnappedEnd
                        |> Grid.getGridLine (model.config |> Config.getGridConfig)
                        |> List.indexedMap
                            (\i coords -> Spot (i + getNewSpotIndex model) True coords)
