module App exposing (..)

-- Others'

import Array


--import Debug

import Html as H
import Html.Attributes as HA
import Keyboard.Extra as KE
import Mouse
import Svg as S
import Svg.Attributes as SA
import Task
import Window


-- Mine

import Grid
import Config exposing (Config)
import ConfigPanel


type alias Spot =
    { index : Int
    , visible : Bool
    , gridCoords : Grid.GridCoords
    }


type alias Model =
    { configPanel : ConfigPanel.Model
    , spots : List Spot
    , windowSize : Window.Size
    , pressedKeys : List KE.Key
    , lockLine :
        Maybe
            { start : Grid.GridCoords
            , unsnappedEnd : Maybe Grid.RealCoords
            }
    , hoverSpotCoords : Maybe Grid.GridCoords
    , optionsOpen : Bool
    }


config : Model -> Config
config =
    .configPanel >> ConfigPanel.config


gridConfig : Model -> Grid.GridConfig
gridConfig =
    config >> Config.getGridConfig


init : ( Model, Cmd Msg )
init =
    { configPanel = ConfigPanel.init
    , spots = []
    , windowSize = Window.Size 0 0
    , pressedKeys = []
    , lockLine = Nothing
    , hoverSpotCoords = Nothing
    , optionsOpen = False
    }
        ! [ Task.perform WindowResize Window.size
          ]


type Msg
    = MouseMove Mouse.Position
    | KeyMsg KE.Msg
    | WindowResize Window.Size
    | ConfigPanelMsg ConfigPanel.Msg


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
                    mouse |> Grid.getGridCoords (gridConfig model)
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
                        { modelAfterInvisibling
                            | lockLine = Just { start = getLastSpotCoords model, unsnappedEnd = Nothing }
                        }
                            ! []

                    Just (KE.KeyDown KE.CharW) ->
                        handleDirectionKey KE.CharW modelAfterInvisibling

                    Just (KE.KeyDown KE.CharA) ->
                        handleDirectionKey KE.CharA modelAfterInvisibling

                    Just (KE.KeyDown KE.CharS) ->
                        handleDirectionKey KE.CharS modelAfterInvisibling

                    Just (KE.KeyDown KE.CharD) ->
                        handleDirectionKey KE.CharD modelAfterInvisibling

                    Just (KE.KeyDown KE.CharQ) ->
                        handleDirectionKey KE.CharQ modelAfterInvisibling

                    Just (KE.KeyDown KE.CharE) ->
                        handleDirectionKey KE.CharE modelAfterInvisibling

                    Just (KE.KeyDown KE.CharL) ->
                        let
                            oldConfig =
                                (config model)
                        in
                            { modelAfterInvisibling
                                | configPanel =
                                    ConfigPanel.setConfig
                                        { oldConfig
                                            | limitSpots = (not oldConfig.limitSpots)
                                        }
                                        modelAfterInvisibling.configPanel
                            }
                                ! []

                    Just (KE.KeyDown KE.Minus) ->
                        { modelAfterInvisibling | spots = [] }
                            ! []

                    Just (KE.KeyDown KE.Escape) ->
                        let
                            configPanelModel =
                                modelAfterInvisibling.configPanel
                        in
                            { modelAfterInvisibling
                                | configPanel =
                                    { configPanelModel
                                        | panelOpen = not modelAfterInvisibling.configPanel.panelOpen
                                    }
                            }
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

        ConfigPanelMsg m ->
            let
                ( configPanelModel, configPanelCmd ) =
                    ConfigPanel.update m model.configPanel
            in
                { model | configPanel = configPanelModel } ! [ Cmd.map ConfigPanelMsg configPanelCmd ]
    )
        |> limitSpots


{-| Limit spot count, leaving extra for the purpose of undo
-}
limitSpots : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
limitSpots ( model, c ) =
    ( { model
        | spots = model.spots |> List.take ((config model).spotCount * 2)
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
    H.div [ HA.id "app" ]
        [ H.div [ HA.id "overlay" ]
            [ ConfigPanel.view model.configPanel |> H.map ConfigPanelMsg
            ]
        , H.div [ HA.class "svgContainer" ]
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
                    (viewSpot (config model))
                    (List.append (getProvisionalSpots model) model.spots
                        |> if (config model).limitSpots then
                            List.take (config model).spotCount
                           else
                            identity
                    )
                    |> (if (config model).newInBack then
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
                 --                        [ SA.x1 (toString (toFloat i * (config model).diameter / 2))
                 --                        , SA.y1 "-4000"
                 --                        , SA.x2 (toString (toFloat i * (config model).diameter / 2))
                 --                        , SA.y2 "4000"
                 --                        , SA.stroke "gray"
                 --                        ]
                 --                        []
                 --                    -- Horizontal line
                 --                    , S.line
                 --                        [ SA.x1 "-4000"
                 --                        , SA.y1 (toString (toFloat i * (config model).diameter / 2 / sqrt 3))
                 --                        , SA.x2 "4000"
                 --                        , SA.y2 (toString (toFloat i * (config model).diameter / 2 / sqrt 3))
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
                (gridConfig model)
                start
    in
        Grid.getCardinalThetas (config model).shape
            |> List.concatMap
                (\theta ->
                    ( lineStartCenter, 100, theta )
                        |> Grid.getGridLine (gridConfig model)
                        |> List.map
                            (\gridCoords ->
                                let
                                    { svg_x, svg_y } =
                                        Grid.getCenter (gridConfig model) gridCoords
                                            |> Grid.realToSvgCoordinates

                                    radius =
                                        (config model).diameter * 0.1
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
                            (gridConfig model)
                            coords
                            |> Grid.realToSvgCoordinates

                    radius =
                        (config model).diameter / 2
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


getLastSpotCoords : Model -> Grid.GridCoords
getLastSpotCoords model =
    case List.head model.spots of
        Nothing ->
            Grid.GridCoords 0 0

        Just spot ->
            spot.gridCoords


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
                    Grid.snapLine (gridConfig model)
                        start
                        unsnappedEnd
                        |> Grid.getGridLine (gridConfig model)
                        |> List.indexedMap
                            (\i coords -> Spot (i + getNewSpotIndex model) True coords)


handleDirectionKey : KE.Key -> Model -> ( Model, Cmd Msg )
handleDirectionKey key model =
    let
        previousCoords =
            getLastSpotCoords model
    in
        case Grid.getAdjacentCoords (gridConfig model) key previousCoords of
            Nothing ->
                model ! []

            Just newCoords ->
                insertNewSpotIfNotRepeat True newCoords model
                    ! []
