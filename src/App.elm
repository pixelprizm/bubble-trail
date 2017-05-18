module App exposing (..)

import Html as H
import Html.Attributes as H
import Mouse
import Char
import Debug
import Keyboard
import Svg as S
import Svg.Attributes as SA


gridSizeInPx : Float
gridSizeInPx =
    20


spotRadius : Float
spotRadius =
    0.9 * gridSizeInPx * 0.5


spotCount : Int
spotCount =
    sizePeriod * colorPeriod



--colorPeriod


colorPeriod : Int
colorPeriod =
    sizePeriod - 1


sizePeriod : Int
sizePeriod =
    12


naturalColors : Bool
naturalColors =
    True


type alias GridCoords =
    { x : Int
    , y : Int
    }


type alias Spot =
    { location : GridCoords
    , index : Int
    }


type alias Model =
    { message : String
    , spots : List Spot
    }


type Msg
    = MouseMove Mouse.Position
    | KeyPress Keyboard.KeyCode


init : ( Model, Cmd Msg )
init =
    { message = "Hello!"
    , spots = []
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        MouseMove position ->
            let
                location : GridCoords
                location =
                    GridCoords
                        (position.x // round gridSizeInPx)
                        (position.y // round gridSizeInPx)
            in
                case List.head model.spots of
                    Nothing ->
                        { model | spots = [ Spot location 0 ] } ! []

                    Just spot ->
                        if spot.location.x == location.x && spot.location.y == location.y then
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
        [ H.div [ H.id "overlay" ] [ H.text model.message ]
        , S.svg [ SA.id "graphics" ]
            (model.spots
                |> List.indexedMap viewSpot
                -- Reverse so that new spots show up over old spots:
                |> List.reverse
            )
        ]


viewSpot : Int -> Spot -> S.Svg Msg
viewSpot index spot =
    S.circle
        [ SA.cx ((toFloat spot.location.x * gridSizeInPx) + (gridSizeInPx / 2) |> toString)
        , SA.cy ((toFloat spot.location.y * gridSizeInPx) + (gridSizeInPx / 2) |> toString)
        , SA.r
            --(spotRadius |> toString)
            (let
                i =
                    (index % sizePeriod) + 1
             in
                --spotRadius * toFloat index * 0.1 |> toString
                spotRadius * toFloat (sizePeriod - i) / toFloat sizePeriod |> toString
             --spotRadius / toFloat i |> toString
            )
        , SA.fill
            ("hsl("
                ++ toString
                    (let
                        ratio : Float
                        ratio =
                            toFloat index / toFloat colorPeriod
                     in
                        ratio
                            * (if naturalColors then
                                360
                               else
                                240
                              )
                    )
                ++ ", 100%, 50%)"
            )
        ]
        []
