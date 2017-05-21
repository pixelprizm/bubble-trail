module App exposing (..)

-- Third-party

import Html as H
import Html.Attributes as H
import Mouse
import Char
import Debug
import Keyboard
import Svg as S
import Svg.Attributes as SA


-- Mine

import CreativeCommonsLicense
import Grid


gridConfig : Grid.GridConfig
gridConfig =
    { diameter = 30
    , shape = Grid.HexPointyTop
    }


spotRadius : Float
spotRadius =
    --0.9 * (gridConfig.diameter / 2)
    (gridConfig.diameter / 2)


spotCount : Int
spotCount =
    --* 6
    --* 3
    --colorPeriod
    --sizePeriod
    sizePeriod * colorPeriod


colorPeriod : Int
colorPeriod =
    --sizePeriod * 2
    sizePeriod - 1


sizePeriod : Int
sizePeriod =
    --120
    12


naturalColors : Bool
naturalColors =
    False


type alias Spot =
    { gridCoords : Grid.GridCoords
    , index : Int
    }


type alias Model =
    { spots : List Spot
    }


type Msg
    = MouseMove Mouse.Position
    | KeyPress Keyboard.KeyCode


init : ( Model, Cmd Msg )
init =
    { spots = []
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        MouseMove position ->
            let
                mouseGridCoords : Grid.GridCoords
                mouseGridCoords =
                    Grid.pixelToGrid gridConfig
                        { x = (toFloat position.x)
                        , y = (toFloat position.y)
                        }
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
    )
        |> limitSpots


limitSpots : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
limitSpots ( model, c ) =
    ( { model | spots = model.spots |> List.take spotCount }, c )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , Keyboard.presses KeyPress
        ]


view : Model -> H.Html Msg
view model =
    H.div [ H.id "app" ]
        [ H.div [ H.id "overlay" ]
            [ CreativeCommonsLicense.view [ H.class "license" ]
            ]
        , S.svg [ SA.id "graphics" ]
            ((model.spots
                |> List.indexedMap viewSpot
                -- Reverse so that new spots show up over old spots:
                |> List.reverse
             )
             --++ ((List.range 0 99)
             --        |> List.map
             --            (\i ->
             --                S.g []
             --                    -- Vertical line
             --                    [ S.line
             --                        [ SA.x1 (toString (toFloat i * gridConfig.diameter / 2))
             --                        , SA.y1 "0"
             --                        , SA.x2 (toString (toFloat i * gridConfig.diameter / 2))
             --                        , SA.y2 "4000"
             --                        , SA.stroke "white"
             --                        ]
             --                        []
             --                    -- Horizontal line
             --                    , S.line
             --                        [ SA.x1 "0"
             --                        , SA.y1 (toString (toFloat i * gridConfig.diameter / 2 / sqrt 3))
             --                        , SA.x2 "4000"
             --                        , SA.y2 (toString (toFloat i * gridConfig.diameter / 2 / sqrt 3))
             --                        , SA.stroke "white"
             --                        ]
             --                        []
             --                    ]
             --            )
             --   )
            )
        ]


viewSpot : Int -> Spot -> S.Svg Msg
viewSpot index spot =
    S.g []
        (let
            i =
                (index % sizePeriod)

            i1 =
                i + 1

            i1WithGaps =
                (index % (sizePeriod * 2))

            useInner =
                False

            outerRadius =
                --case i of
                --    0 ->
                --        spotRadius
                --    _ ->
                --        spotRadius / 2
                --spotRadius * toFloat index * 0.1
                spotRadius * toFloat (sizePeriod - i) / toFloat sizePeriod

            --spotRadius
            --spotRadius * toFloat (sizePeriod - i1WithGaps) / toFloat sizePeriod
            --spotRadius / toFloat i1
            --spotRadius
            innerRadius =
                --outerRadius * 0.8
                --spotRadius / toFloat i1
                0.8 * spotRadius * toFloat (sizePeriod - i) / toFloat sizePeriod

            { x, y } =
                Grid.gridToCenterPixel gridConfig spot.gridCoords

            colorRatio =
                toFloat index / toFloat colorPeriod

            hue =
                colorRatio
                    * (if naturalColors then
                        240
                       else
                        360
                      )

            outerHsl =
                { h = hue

                --{ h = 0
                , s = 100

                --, s = 0
                --, l = 70
                , l = 50

                --, l = 0
                }

            innerHsl =
                { h = hue

                --{ h = hue
                --, s = 0
                , s = 100

                --, l = 0
                --, l = 70
                , l = 50
                }

            hslToString { h, s, l } =
                "hsl(" ++ toString h ++ "," ++ toString s ++ "%," ++ toString l ++ "%)"
         in
            ([ S.circle
                [ SA.cx (x |> toString)
                , SA.cy (y |> toString)
                , SA.r (outerRadius |> toString)
                , SA.fill (outerHsl |> hslToString)
                ]
                []
             ]
                ++ if useInner then
                    [ S.circle
                        [ SA.cx (x |> toString)
                        , SA.cy (y |> toString)
                        , SA.r (innerRadius |> toString)
                        , SA.fill (innerHsl |> hslToString)
                        ]
                        []
                    ]
                   else
                    []
            )
        )
