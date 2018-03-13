module Pages.NewPersonsPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople exposing (HasCheckIn)
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import NewPerson as NewPerson


type alias Msg =



update : Msg -> HasNewPersonsPage m -> ( HasNewPersonsPage m, Cmd Msg )
update msg mdl =

view : HasNewPersonsPage m -> Html Msg
view mdl =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ NewPerson.newFamilysView mdl
                ]
            ]
        ]
