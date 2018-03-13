module Pages.HomePage exposing (..)

import Html as Html
    exposing
        ( Html
        , br
        , button
        , div
        , h1
        , h3
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
    }


view : Model -> Html Msg
view mdl =
    let
        brf =
            br [] []
    in
    div []
        [ div [ class "text-center" ]
            [ h1 [] [ text "Welcome to" ]
            , h3 [] [ text mdl.eventInfo.eventName ]
            , h3 [] [ text "at" ]
            , h3 [] [ text "Mountain View Church" ]
            , brf
            , brf
            , p [] [ text "Let's get you checked in!" ]
            , continueButton False
                [ text "Check-in  "
                , Html.i [ class "fas fa-sign-in-alt" ] []
                ]
            ]
        ]
