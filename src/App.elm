module App exposing (..)

-- Others'

import Array
import Char
import Debug
import Html as H
import Html.Attributes as H
import Keyboard
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
    --Config.rainbowPulseSquare
    --Config.rainbowPulseHex
    --Config.growShrinkSquare
    --Config.growShrinkHex
    --Config.justGrowHex
    Config.justGrowSquare


viewportToCenteredCoordinates : Window.Size -> Mouse.Position -> Grid.PixelCoords
viewportToCenteredCoordinates { width, height } { x, y } =
    Grid.PixelCoords
        (toFloat x - (toFloat width / 2))
        (toFloat y - (toFloat height / 2))


centeredToViewportCoordinates : Window.Size -> Grid.PixelCoords -> Mouse.Position
centeredToViewportCoordinates { width, height } { x, y } =
    Mouse.Position
        (floor x + (width // 2))
        (floor y + (height // 2))


type alias Spot =
    { gridCoords : Grid.GridCoords
    , index : Int
    }


type alias Model =
    { spots : List Spot
    , windowSize : Window.Size
    }


type Msg
    = MouseMove Mouse.Position
    | KeyPress Keyboard.KeyCode
    | WindowResize Window.Size


init : ( Model, Cmd Msg )
init =
    { spots = []
    , windowSize = Window.Size 0 0
    }
        ! [ Task.perform WindowResize Window.size
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        MouseMove mousePosition ->
            let
                mouseGridCoords : Grid.GridCoords
                mouseGridCoords =
                    Grid.pixelToGrid (chosenConfig |> Config.getGridConfig)
                        (viewportToCenteredCoordinates model.windowSize mousePosition)
            in
                case List.head model.spots of
                    Nothing ->
                        { model | spots = [ Spot mouseGridCoords 0 ] } ! []

                    Just spot ->
                        if spot.gridCoords == mouseGridCoords then
                            model ! []
                        else
                            { model
                                | spots = (Spot mouseGridCoords (spot.index + 1)) :: model.spots
                            }
                                ! []

        KeyPress keyCode ->
            case Debug.log "code" (Char.fromCode keyCode) of
                'e' ->
                    model ! []

                _ ->
                    model ! []

        WindowResize size ->
            { model | windowSize = size } ! []
    )
        |> limitSpots


limitSpots : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
limitSpots ( model, c ) =
    ( { model | spots = model.spots |> List.take chosenConfig.spotCount }, c )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , Keyboard.presses KeyPress
        , Window.resizes WindowResize
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
                    (model.spots)
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
            { x, y } =
                Grid.gridToCenterPixel (chosenConfig |> Config.getGridConfig) spot.gridCoords

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
                [ SA.cx (x |> toString)
                , SA.cy (y |> toString)
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
