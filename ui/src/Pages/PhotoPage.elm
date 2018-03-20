module Pages.PhotoPage exposing (..)

import FindPeople as Find
import Html as Html exposing (Html, br, div, h1, h2, h3, h4, h5, p, text)
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
                    We have a photographer here taking photos of the event for
                    Mountain View Chuch. The photos will be used for our website
                    and other online materials.
                    """
                ]
            , h5 [] [ text "Do You Mind If You're In A Photo?" ]
            , br [] []
            , h4 [] [ text "We Don't Mind" ]
            ]
        , div [ class "grow-6 w-100" ]
            [ Html.map FindMsg <| Find.waitingPersonsWithPhotoSelect mdl
            ]
        , navButtons
            Router.Cart
            False
            Router.Safety
        ]
