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


gridSizeInPx : Float
gridSizeInPx =
    20


spotRadius : Float
spotRadius =
    0.9 * gridSizeInPx * 0.5 * 2


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


gridShape : GridShape
gridShape =
    HexPointyTop


pixelToGrid : GridShape -> PixelCoords -> GridCoords
pixelToGrid shape { x, y } =
    case shape of
        Square ->
            GridCoords
                (x / gridSizeInPx |> round)
                (y / gridSizeInPx |> round)

        HexFlatTop ->
            GridCoords
                (x * 2 / 3 / gridSizeInPx |> round)
                ((-x / 3 + sqrt 3 / 3 * y) / gridSizeInPx |> round)

        HexPointyTop ->
            GridCoords
                ((x * sqrt 3 / 3 - y / 3) / gridSizeInPx |> round)
                (y * 2 / 3 / gridSizeInPx |> round)


gridToCenterPixel : GridShape -> GridCoords -> PixelCoords
gridToCenterPixel shape { q, r } =
    case shape of
        Square ->
            PixelCoords
                ((toFloat q * gridSizeInPx) + (gridSizeInPx / 2))
                ((toFloat r * gridSizeInPx) + (gridSizeInPx / 2))

        HexFlatTop ->
            PixelCoords
                (gridSizeInPx * toFloat q * 3 / 2)
                (gridSizeInPx * sqrt 3 * (toFloat r + toFloat q / 2))

        HexPointyTop ->
            PixelCoords
                (gridSizeInPx * sqrt 3 * (toFloat q + toFloat r / 2))
                (gridSizeInPx * toFloat r * 3 / 2)


type alias Spot =
    { location : GridCoords
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
                location : GridCoords
                location =
                    pixelToGrid gridShape
                        (PixelCoords
                            (toFloat position.x)
                            (toFloat position.y)
                        )
            in
                case List.head model.spots of
                    Nothing ->
                        { model | spots = [ Spot location 0 ] } ! []

                    Just spot ->
                        if spot.location == location then
                            model ! []
                        else
                            { model
                                | spots = (Spot location (spot.index + 1)) :: model.spots
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
            (model.spots
                |> List.indexedMap viewSpot
                -- Reverse so that new spots show up over old spots:
                |> List.reverse
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
                spotRadius * toFloat (sizePeriod - i1) / toFloat sizePeriod

            --spotRadius * toFloat (sizePeriod - i1WithGaps) / toFloat sizePeriod
            --spotRadius / toFloat i1
            --spotRadius
            innerRadius =
                --outerRadius * 0.8
                --spotRadius / toFloat i1
                0.8 * spotRadius * toFloat (sizePeriod - i) / toFloat sizePeriod

            { x, y } =
                gridToCenterPixel gridShape spot.location

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
