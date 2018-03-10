module Pages.HomePage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Text as Text
import FindPeople exposing (HasCheckIn)
import Router as Router
import Html as Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h2
        , h5
        , h3
        , h1
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class)

--type alias HasHomePage m =
brf = br [][]
view: String -> Html Router.Msg
view eventName = div [] [
          div [class "text-center"] [
              h1 [] [text "Welcome to"]
            , h3 [] [text eventName]
            , h3 [] [text "at"]
            , h3 [] [text "Mountain View Church"]
              , brf
              , brf
              , p [] [text "Let's get you checked in!"]
              , Button.button
                 [ Button.onClick <| Router.SetRoute Router.Search
                 , Button.large
                 , Button.outlineSuccess
                 ]
                 [
                   text "Check-in  "
                   , Html.i [ class "fas fa-sign-in-alt" ] []
                 ]
          ]



          ]
