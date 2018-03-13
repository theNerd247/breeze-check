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
        disableCheckin =
            Dict.size mdl.waitingCheckIn <= 0

        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    []
                ]

        waiting =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.attrs [ class "text-center" ] ] <|
                    if disableCheckin then
                        [ h4 [] [ text "You Haven't Selected Anyone Yet" ]
                        , goToPageButton Router.Search [ text "Go back" ]
                        ]
                    else
                        [ h4 [] [ text "You're checking in" ]
                        , Html.map FindMsg <| Find.waitingPersonsWithEdit mdl
                        ]
                ]
    in
    div []
        [ waiting
        , continueButton disableCheckin [ text "Continue" ]
        ]
