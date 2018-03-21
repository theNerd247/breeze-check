module Pages.SelectPage exposing (..)

import Bootstrap.Button as Button
import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, br, div, h2, h3, h5, p, span, text)
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
                    [ h5 [] [ text "Now, Select Who You'd Like To Check-in" ]
                    ]
            else
                text ""

        results =
            div [ class "d-flex flex-column align-items-center" ]
                [ title
                , Html.map FindMsg <| Find.searchResultsView mdl
                , br [] []
                , p [ class "text-secondary text-center w-50" ]
                    [ span [] [ text "If you can't find your family click" ]
                    , span [] [ text """ "Add Missing Members" """ ]
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
            Button.button
                [ Button.onClick <| RouterMsg <| Router.SetRoute <| Router.Cart
                , Button.disabled <| nselected <= 0
                , Button.block
                , Button.success
                ]
                [ text <| "Next"
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
        , div [ class "grow-1 pb-3 w-100" ] [ results ]
        , br [] []
        , div [ class "grow-6 w-75" ] [ nextButton ]
        ]
