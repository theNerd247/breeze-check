module Pages.SelectPage exposing (..)

import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, br, div, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Selected
    , pageTitle = "Search Results"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        title =
            if mdl.personNotFound == Nothing then
                div [ class "text-center" ]
                    [ h4 [] [ text "Now, Select Who You'd Like To Check-in" ]
                    ]
            else
                text ""

        results =
            div [ class "d-flex flex-column w-100 align-items-center" ]
                [ title
                , Html.map FindMsg <| Find.searchResultsView mdl
                , br [] []
                , p [ class "text-secondary text-center" ]
                    [ text
                        """
                        If you can't find your family click the button below
                        """
                    ]
                , notFoundButton
                ]

        notFoundButton =
            goToPageAndThenButton
                Router.NewPersons
                [ text "Add Missing Members" ]
                NewPerson.resetNewPersons

        nselected =
            Dict.size mdl.waitingCheckIn

        nextButton =
            continueButton (nselected <= 0)
                Router.Cart
                [ text <| "Check-In (" ++ toString nselected ++ ")"
                ]

        searchBar =
            div [ class "w-100 grow-1 d-flex justify-content-center align-items-center" ]
                [ div [ class "w-100 order-1" ]
                    [ Html.map FindMsg <|
                        Find.searchPersonsForm mdl
                    ]
                ]
    in
    pageWrapper "justify-content-start"
        [ searchBar
        , div [ class "grow-1 pb-3" ] [ results ]
        , div [ class "grow-6" ] [ nextButton ]
        ]
