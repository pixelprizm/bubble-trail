module App exposing (..)

import Html as H
import Html.Attributes as H
import Mouse
import Task
import Svg as S
import Svg.Attributes as S


type alias Model =
    { message : String
    }


type Msg
    = MouseMove Mouse.Position


init : ( Model, Cmd Msg )
init =
    { message = "Hello!"
    }
        ! [ ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove position -> model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMove
        ]


view : Model -> H.Html Msg
view model =
    H.div [ H.id "app" ]
        [ H.div [ H.id "overlay"] [ H.text model.message ]
        , S.svg [ ] []
        ]
