module Pages.EditFamilyInfoPage exposing (..)

import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.EditFamilyInfo
    , pageTitle = "New Attendees"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    pageWrapper ""
        [ div [ class "text-center" ]
            [ h4 [] [ text "Enter Your Family Members Below" ]
            , Html.map NewPersonMsg <| NewPerson.newPersonInfoForm mdl
            ]
        , navButtons Router.NewPersons False Router.Selected
        ]
