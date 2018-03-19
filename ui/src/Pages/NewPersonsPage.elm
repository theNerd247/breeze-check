module Pages.NewPersonsPage exposing (..)

import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.NewPersons
    , pageTitle = "New Attendees"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    pageWrapper ""
        [ div [ class "grow-1 text-center" ]
            [ h4 [] [ text "Add You're Family" ]
            , Html.map NewPersonMsg <| NewPerson.newPersonsForm mdl
            , navButtonsWrapper
                (backButton Router.Selected)
                (goToPageAndThenButton
                    Router.EditFamilyInfo
                    [ text "Next" ]
                    NewPerson.resetNewPersonInfos
                )
            ]
        ]
