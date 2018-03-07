module NewPersonsPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import NewPerson as NewPerson


newPersonsView : NewPerson.HasNewFamilies m -> (NewPerson.Msg -> msg) -> Html msg
newPersonsView mdl f =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ Html.map f <| NewPerson.newFamilysView mdl
                ]
            ]
        ]
