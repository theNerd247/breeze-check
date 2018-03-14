module Pages.EditFamilyInfo exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dict as Dict
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.EditFamilyInfo
    , nextPageRoute = Router.Selected
    , pageTitle = "New Attendees"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12, Col.attrs [ class "text-center" ] ]
                [ h4 [] [ text "Enter Your Family Members Below" ]
                , Html.map NewPersonMsg <| NewPerson.newPersonInfoForm mdl
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ if Dict.size mdl.newPersons > 0 then
                    Html.map NewPersonMsg <| NewPerson.createAttendeesButton
                  else
                    text ""
                ]
            ]
        ]
