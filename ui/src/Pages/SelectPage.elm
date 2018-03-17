module Pages.SelectPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, br, div, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Selected
    , pageTitle = "Results"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h4 [] [ text "Now, Select Who You'd Like To Check-in" ]
                    ]
                ]

        results =
            div [ class "d-flex flex-column w-100 align-items-center" ]
                [ Html.map FindMsg <| Find.searchResultsView mdl
                , br [] []
                , p [ class "text-secondary" ] [ text "(If you can't find your family click here)" ]
                , notFoundButton
                ]

        notFoundButton =
            goToPageButton Router.NewPersons [ text "Create My Family" ]

        nselected =
            Dict.size mdl.waitingCheckIn

        nextButton =
            continueButton (nselected <= 0)
                Router.Cart
                [ text <| "Continue (" ++ toString nselected ++ ")"
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
        , div [ class "grow-2" ] [ results ]
        , div [ class "grow-6" ] [ nextButton ]
        ]
