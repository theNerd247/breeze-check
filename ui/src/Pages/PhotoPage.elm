module Pages.PhotoPage exposing (..)

import FindPeople as Find
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class)
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
    pageWrapper ""
        [ div [ class "grow-1 text-center w-100" ]
            [ h1 [] [ text "Photo Waiver" ]
            , p []
                [ text
                    """
                    photo page
                    """
                ]
            ]
        , div [ class "grow-6 w-100" ]
            [ Html.map FindMsg <| Find.waitingPersonsWithPhotoSelect mdl
            ]
        , navButtons
            Router.Cart
            False
            Router.Safety
        ]
