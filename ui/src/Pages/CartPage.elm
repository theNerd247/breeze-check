module Pages.CartPage exposing (..)

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
    { pageRoute = Router.Cart
    , nextPageRoute = Router.Photo
    , pageTitle = "Cart"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h3 [] [ text "You're checking in" ]
                    ]
                ]

        waiting =
            let
                a =
                    if not <| Dict.isEmpty mdl.waitingCheckIn then
                        [ class "pb-3" ]
                    else
                        []
            in
            Grid.row [ Row.attrs a, Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ Html.map FindMsg <| Find.waitingPersonsWithEdit mdl
                    ]
                ]
    in
    div []
        [ title
        , waiting
        , continueButton False [ text "Continue" ]
        ]
