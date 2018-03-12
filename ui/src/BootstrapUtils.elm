module BootstrapUtils exposing (..)

import Bootstrap.Button as Button
import Html as Html exposing (Html, text)


deleteButton : msg -> Html msg
deleteButton msg =
    Button.button
        [ Button.onClick msg
        , Button.danger
        ]
        [ text "-" ]
