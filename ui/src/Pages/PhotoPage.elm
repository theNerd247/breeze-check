module Pages.PhotoPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople as Find
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Photo
    , pageTitle = "Photo Waiver"
    , pageView = view
    , showInNavbar = False
    }


view : Model -> Html Msg
view mdl =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ h1 [] [ text "May We Take You're Picture Please?" ]
                , p []
                    [ text
                        """
                        photo page
                        """
                    ]
                , p []
                    [ Html.b [] [ text "We won't publish any names or contant information!" ]
                    ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col []
                [ Html.map FindMsg <| Find.waitingPersonsWithPhotoSelect mdl ]
            ]
        , navButtons
            Router.Cart
            False
            Router.Safety
        ]
