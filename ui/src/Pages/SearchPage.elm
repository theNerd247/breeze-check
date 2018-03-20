module Pages.SearchPage exposing (..)

import FindPeople as Find
import Html as Html exposing (Html, br, div, h2, h4, p, text)
import Html.Attributes exposing (class)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Search
    , pageTitle = "Search"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        title =
            div
                [ class "text-center" ]
                [ h4 [] [ text "Let's Start By Finding Your Family" ]
                , br [] []
                , p []
                    [ text
                        """
                        If you've brought visitors with you don't worry! We'll
                        make sure they check-in later.
                        """
                    ]
                ]

        searchForm =
            Html.map FindMsg <| Find.searchPersonsForm mdl
    in
    pageWrapper "justify-content-around"
        [ div [ class "grow-1 d-flex align-items-center" ] [ title ]
        , div [ class "grow-6 w-80" ] [ searchForm ]
        ]
