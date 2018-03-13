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
    in
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.attrs [ class "text-center" ] ] <|
                if disableCheckin then
                    [ h4 [] [ text "You Haven't Selected Anyone Yet" ]
                    ]
                else
                    [ h4 [] [ text "Make Sure Everyone is Here" ]
                    , Find.waitingPersons mdl
                    ]
            ]
        , Grid.row [ Row.centerXs, Row.attrs [ class "mb-3" ] ]
            [ Grid.col [ Col.xsAuto ]
                [ goToPageButton
                    (if disableCheckin then
                        Router.Search
                     else
                        Router.Selected
                    )
                    [ text "Go Back" ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ continueButton disableCheckin [ text "Continue" ]
                ]
            ]
        ]
