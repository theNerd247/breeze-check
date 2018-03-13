module Pages.NewPersonsPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.NewPersons
    , nextPageRoute = Router.Selected
    , pageTitle = "New Family"
    , pageView = view
    , showInNavbar = False
    }


view : Model -> Html Msg
view mdl =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ Html.map NewPersonMsg <| NewPerson.newPersonsForm mdl
                ]
            ]
        ]
