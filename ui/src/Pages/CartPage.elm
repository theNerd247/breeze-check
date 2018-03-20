module Pages.CartPage exposing (..)

import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Cart
    , pageTitle = "Check-In List"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        disableCheckin =
            Dict.size mdl.waitingCheckIn <= 0
    in
    pageWrapper ""
        [ div [ class "w-100 text-center" ] <|
            if disableCheckin then
                [ h4 [] [ text "You Haven't Selected Anyone Yet" ]
                ]
            else
                [ h4 [] [ text "Make Sure Everyone is Here" ]
                , Html.map FindMsg <| Find.waitingPersons mdl
                ]
        , p [ class "text-center text-secondary" ]
            [ text
                """If you've brought visitors and they aren't here then
                    click "Check-In More People"
                 """
            ]
        , div [ class "d-flex flex-row justify-content-center pb-3" ]
            [ goToPageAndThenButton
                Router.Selected
                [ text "Check-In More People" ]
                Find.resetFind
            ]
        , navButtons Router.Selected disableCheckin Router.Photo
        ]
