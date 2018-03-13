module Pages.HomePage exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Text as Text
import FindPeople exposing (HasCheckIn)
import Html as Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h1
        , h2
        , h3
        , h5
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class)
import Pages exposing (..)
import Router as Router


--type alias HasHomePage m =


config : Config m
config =
    Pages.config
        |> pageRoute Router.Cart
        |> nextPage Router.Photo
        |> title "Home"
        |> pageView view


brf =
    br [] []


view : Model m -> Html Msg
view eventName =
    div []
        [ div [ class "text-center" ]
            [ h1 [] [ text "Welcome to" ]
            , h3 [] [ text eventName ]
            , h3 [] [ text "at" ]
            , h3 [] [ text "Mountain View Church" ]
            , brf
            , brf
            , p [] [ text "Let's get you checked in!" ]
            , Button.button
                [ Button.onClick <| Router.SetRoute Router.Search
                , Button.large
                , Button.outlineSuccess
                ]
                [ text "Check-in  "
                , Html.i [ class "fas fa-sign-in-alt" ] []
                ]
            ]
        ]
