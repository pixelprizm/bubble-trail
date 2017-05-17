module Main exposing (..)

import App
import Html


main =
    Html.program
        { view = App.view
        , init = App.init
        , update = App.update
        , subscriptions = App.subscriptions
        }
