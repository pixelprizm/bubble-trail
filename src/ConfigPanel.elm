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
                [ H.h1 [ HA.class "ConfigPanelTitle" ] [ H.text "Bubble Trail" ]
                , H.h2 [ HA.class "ConfigPanelSubtitle" ] [ H.text "by eric gauderman" ]
                , H.div [ HA.class "ConfigPanelOptionsBody" ]
                    [ H.h3 [ HA.class "ConfigPanelHeading" ] [ H.text "controls" ]
                    , viewKeyboardControls
                    , H.h3 [ HA.class "ConfigPanelHeading" ] [ H.text "options" ]
                    , viewOptions
                    , H.div [ HA.class "ConfigPanelLicenseContainer" ] [ CreativeCommonsLicense.view [] ]
                    ]
                ]
            ]
        ]


viewKeyboardControls =
    let
        keyboardControlw =
            [ ( [ "space" ], "move cursor without drawing", "" )
            , ( [ "shift" ], "snap to line", "" )
            , ( [ "esc" ], "toggle panel", "" )
            , ( [ "backspace" ], "delete last bubble", "delete & tab also do this" )
            , ( [ "-" ], "clear all bubbles", "" )
            , ( [ "x" ], "draw an invisible bubble", "" )
            , ( [ "h" ], "change grid shape", "square, hex, hex vertical" )
            , ( [ "w", "a", "s", "d" ], "draw one bubble in a direction", "also q & e for hex" )
            ]
    in
        H.div []
            (keyboardControlw
                |> List.map
                    (\( keys, description, sub ) ->
                        H.div [ HA.class "ConfigPanelKeyboardShortcutRow" ]
                            [ H.div [ HA.class "key" ] (keys |> List.map (\key -> H.span [] [ H.text key ]))
                            , H.div [ HA.class "description" ]
                                [ H.span []
                                    [ H.span [] [ H.text description ]
                                    , H.br [] []
                                    , H.span [ HA.class "sub" ] [ H.text sub ]
                                    ]
                                ]
                            ]
                    )
            )


viewOptions =
    H.div []
        [ H.text "hi"
        ]
