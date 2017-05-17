module App exposing (..)

import Html as H
import Html.Attributes as H
import Mouse
import Svg as S
import Svg.Attributes as S


gridSizeInPx : Int
gridSizeInPx =
    20


spotDiameter : Int
spotDiameter =
    round 0.9 * gridSizeInPx


spotCount : Int
spotCount =
    64


type alias GridCoords =
    { x : Int
    , y : Int
    }


type alias Spot =
    { location : GridCoords
    }


type alias Model =
    { message : String
    , spots : List Spot
    }


type Msg
    = MouseMove Mouse.Position


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
                        (position.x // gridSizeInPx)
                        (position.y // gridSizeInPx)
            in
                case List.head model.spots of
                    Nothing ->
                        { model | spots = [ Spot location ] } ! []

                    Just spot ->
                        if spot.location.x == location.x && spot.location.y == location.y then
                            model ! []
                        else
                            { model
                                | spots = (Spot location) :: model.spots
                            }
                                ! []
    )
        |> limitSpots


limitSpots : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
limitSpots ( model, c ) =
    ( { model | spots = model.spots |> List.take spotCount }, c )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        ]


view : Model -> H.Html Msg
view model =
    H.div [ H.id "app" ]
        [ H.div [ H.id "overlay" ] [ H.text model.message ]
        , S.svg [ S.id "graphics" ]
            (model.spots
                |> List.indexedMap viewSpot
            )
        ]


viewSpot : Int -> Spot -> S.Svg Msg
viewSpot index spot =
    S.circle
        [ S.cx ((spot.location.x * gridSizeInPx) + (gridSizeInPx // 2) |> toString)
        , S.cy ((spot.location.y * gridSizeInPx) + (gridSizeInPx // 2) |> toString)
        , S.r (spotDiameter // 2 |> toString)
        , S.fill
            ("hsl("
                ++ toString
                    (let
                        ratio : Float
                        ratio =
                            toFloat index / toFloat spotCount
                     in
                        ratio * 240
                    )
                ++ ", 100%, 50%)"
            )
        ]
        []
