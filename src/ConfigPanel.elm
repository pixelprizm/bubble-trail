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
    { panelOpen = False

    --, config = Config.justGrowHex Grid.PointyTop
    --, config = Config.justGrowSquare
    --, config = Config.growShrinkSquare
    --, config = Config.growShrinkHex Grid.FlatTop
    --, config = Config.rainbowPulseHex Grid.PointyTop
    , config = Config.rainbowPulseSquare
    }


config : Model -> Config
config =
    .config


setConfig : Config -> Model -> Model
setConfig c model =
    { model | config = c }


cycleGridShape : Model -> Model
cycleGridShape model =
    let
        oldConfig =
            model.config
    in
        { model
            | config =
                { oldConfig
                    | shape =
                        case (config model).shape of
                            Grid.Square ->
                                Grid.Hex Grid.PointyTop

                            Grid.Hex Grid.PointyTop ->
                                Grid.Hex Grid.FlatTop

                            Grid.Hex Grid.FlatTop ->
                                Grid.Square
                }
        }


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
                    , viewKeyboardControls model

                    --, H.h3 [ HA.class "ConfigPanelHeading" ] [ H.text "options" ]
                    --, viewOptions model
                    , H.div [ HA.class "ConfigPanelLicenseRow" ]
                        [ H.div [ HA.class "ConfigPanelLicenseContainer" ] [ CreativeCommonsLicense.view [] ]
                        ]
                    ]
                ]
            ]
        ]


viewKeyboardControls : Model -> H.Html Msg
viewKeyboardControls model =
    let
        keyboardControlw =
            [ ( [ "space" ], "hold to move cursor without drawing", "" )
            , ( [ "shift" ], "hold to snap to line", "" )
            , ( [ "backspace" ], "delete last bubble", "delete & tab also do this" )
            , ( [ "-" ], "clear all bubbles", "be careful, can't be undone" )

            --, ( [ "w", "a", "s", "d" ], "draw one bubble in a direction", "also q & e for hex" )
            , ( [ "x" ], "draw an invisible bubble", "" )
            , ( [ "h" ], "change grid shape", "square, hex, hex vertical" )
            , ( [ "esc" ], "toggle panel", "" )
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


viewOptions : Model -> H.Html Msg
viewOptions model =
    H.div []
        [ H.text "hi"
        ]
