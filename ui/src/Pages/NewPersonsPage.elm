module Pages.NewPersonsPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import NewPerson as NewPerson


type alias Msg =
    NewPerson.Msg


type alias HasNewPersonsPage m =
    NewPerson.HasNewFamilies m


view : HasNewPersonsPage m -> Html Msg
view mdl =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ NewPerson.newFamilysView mdl
                ]
            ]
        ]
