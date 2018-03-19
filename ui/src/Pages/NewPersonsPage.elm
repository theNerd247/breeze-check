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
                (goToPageAndThenButton Router.EditFamilyInfo [ text "Next" ] <|
                    NewPerson.resetCurrentInfoEdit
                        << NewPerson.resetNewPersons
                )
            ]
        ]


nav =
    div []
        [ personForm np
        , div [ class "grow-6 d-flex flex-row justify-content-center w-100" ]
            [ div [ class "grow-auto px-3" ] [ l ]
            , div [ class "grow-auto px-3" ] [ navButton NextInfoEdit isLast "Next Family" ]
            ]
        ]


nextButton =
    Button.button
        [ if isLast then
            Button.onClick CreateNewAttendees
          else
            NextInfoEdit
        , Button.outlinePrimary
        ]
        [ text <|
            if isLast then
                "Save"
            else
                "Next Family"
        ]


prevButton =
    Button.button
        [ if isLast then
            Button.onClick CreateNewAttendees
          else
            NextInfoEdit
        , Button.outlinePrimary
        ]
        [ text <|
            if isLast then
                "Save"
            else
                "Next Family"
        ]
