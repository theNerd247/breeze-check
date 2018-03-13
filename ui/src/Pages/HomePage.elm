module Pages.HomePage exposing (..)

import Html as Html
    exposing
        ( Html
        , br
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , p
        , text
        )
import Html.Attributes exposing (class)
import Pages exposing (..)
import Router as Router


--type alias HasHomePage m =


config : Config
config =
    { pageRoute = Router.Home
    , nextPageRoute = Router.Search
    , pageTitle = "Home"
    , pageView = view
    , showInNavbar = False
    }


view : Model -> Html Msg
view mdl =
    let
        brf =
            br [] []
    in
    div []
        [ div [ class "text-center" ]
            [ h2 [] [ text "Welcome to" ]
            , h4 [] [ text "Mountain View Church's" ]
            , h3 [] [ text mdl.eventInfo.eventName ]
            , brf
            , brf
            , p [] [ text "Let's get you checked in!" ]
            , continueButton False
                [ text "Check-in  "
                , Html.i [ class "fas fa-sign-in-alt" ] []
                ]
            ]
        ]
