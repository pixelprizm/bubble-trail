module CreativeCommonsLicense exposing (..)

import Html as H
import Html.Attributes as H


view : List (H.Attribute msg) -> H.Html msg
view attributes =
    H.a
        ([ H.rel "license"
         , H.href "http://creativecommons.org/licenses/by-nc-sa/4.0/"
         , H.target "_blank"
         , H.style [ ( "line-height", "0" ) ]
         ]
            ++ attributes
        )
        [ H.img
            [ H.alt "Creative Commons License"
            , H.style [ ( "border-width", "0" ) ]
            , H.src "https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png"
            ]
            []
        ]
