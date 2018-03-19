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
        , navButtonsNewPerson
        ]


navButtonsNewPerson : Html Msg
navButtonsNewPerson =
    div [ class "grow-6 d-flex flex-row justify-content-center w-100" ]
        [ div [ class "grow-auto px-3" ] [ goToPageButton Router.NewPersons [ text "Back" ] ]
        , div [ class "grow-auto px-3" ] [ Html.map NewPersonMsg <| NewPerson.createAttendeesButton ]
        ]
