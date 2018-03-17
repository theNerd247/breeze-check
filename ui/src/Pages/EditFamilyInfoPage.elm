module Pages.EditFamilyInfoPage exposing (..)

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
    pageWrapper ""
        [ div [ class "text-center" ]
            [ h4 [] [ text "Enter Your Family Members Below" ]
            , Html.map NewPersonMsg <| NewPerson.newPersonInfoForm mdl
            ]
        , div [ class "grid-auto d-flex flex-row justify-content-around" ]
            [ goToPageButton Router.NewPersons [ text "Back" ]
            , if Dict.isEmpty mdl.newPersons then
                Html.map NewPersonMsg <| NewPerson.createAttendeesButton
              else
                text ""
            ]
        ]
