module Pages.SelectPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, text)
import Html.Attributes exposing (class, for, style)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Selected
    , nextPageRoute = Router.Cart
    , pageTitle = "Results"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        hasSelectablePeople =
            Dict.size mdl.foundPeople

        nselected =
            Dict.size mdl.waitingCheckIn

        title =
            case hasSelectablePeople of
                0 ->
                    text ""

                _ ->
                    Grid.row [ Row.centerXs ]
                        [ Grid.col [ Col.xs12 ]
                            [ h4 [ class "text-center" ] [ text "Now, Select Who You'd Like To Check-in" ]
                            ]
                        ]

        results =
            Grid.row [ Row.attrs [ class "pb-3" ], Row.centerXs ]
                [ Grid.col [ Col.xs12 ] <|
                    [ Html.map FindMsg <| Find.searchResultsView mdl
                    ]
                , Grid.col [ Col.xsAuto ] [ notFoundButton ]
                ]

        notFoundButton =
            goToPageButton Router.NewPersons [ text "Add A New Person" ]
    in
    div []
        [ Html.map FindMsg <| Find.searchPersonsForm mdl
        , title
        , results
        , continueButton (nselected <= 0)
            [ text <| "Continue (" ++ toString nselected ++ ")"
            ]
        ]
