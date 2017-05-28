module ConfigPanel exposing (..)

-- Others'

import Html as H
import Html.Attributes as HA
import Html.Events as HE


-- Mine

import Config exposing (Config)
import Grid
import CreativeCommonsLicense


type alias Model =
    { panelOpen : Bool
    , config : Config
    }


init : Model
init =
    { panelOpen = True

    --, Config.justGrowHex Grid.PointyTop
    --, Config.justGrowSquare
    --, Config.growShrinkSquare
    --, Config.growShrinkHex Grid.FlatTop
    --, Config.rainbowPulseSquare
    , config = Config.rainbowPulseHex Grid.PointyTop
    }


config : Model -> Config
config =
    .config


setConfig : Config -> Model -> Model
setConfig c model =
    { model | config = c }


type Msg
    = SetConfig Config
    | SetPanelOpen Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetConfig c ->
            { model
                | config = c
            }
                ! []

        SetPanelOpen o ->
            { model
                | panelOpen = o
            }
                ! []


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.class
            ("ConfigPanelContainer"
                ++ if model.panelOpen then
                    ""
                   else
                    " ConfigPanelClosed"
            )
        ]
        [ H.div
            [ HA.class "ConfigPanel"
            ]
            [ H.div
                [ HA.class "ConfigPanelToggleContainer"
                , HE.onClick <| SetPanelOpen <| not model.panelOpen
                ]
                [ H.i [ HA.class "material-icons ConfigPanelToggle opener" ] [ H.text "menu" ]
                , H.i [ HA.class "material-icons ConfigPanelToggle closer" ] [ H.text "arrow_forward" ]
                ]
            , H.div
                [ HA.class "ConfigPanelOptionsArea" ]
                [ H.h1 [ HA.class "ConfigPanelTitle" ]
                    [ H.text "Bubble Trail" ]
                , H.h2 [ HA.class "ConfigPanelSubtitle" ]
                    [ H.text "by eric gauderman" ]
                , H.div [ HA.class "ConfigPanelOptionsBody" ]
                    [ H.text "hello world" ]
                ]
            , CreativeCommonsLicense.view [ HA.class "license" ]
            ]
        ]
