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
            , p [ class "text-justify" ]
                [ text
                    """
                    We have a photographer here taking photos of the event for
                    Mountain View Chuch. The photos will be used for our website
                    and other online materials.
                    """
                ]
            , br [] []
            , h5 [] [ text "It's Ok If We're In A Photo" ]
            , p [ class "text-center text-secondary" ]
                [ text "deselect people below if you don't want them to appear in a photo" ]
            ]
        , div [ class "grow-6 w-100" ]
            [ Html.map FindMsg <| Find.waitingPersonsWithPhotoSelect mdl
            ]
        , navButtons
            Router.Cart
            False
            Router.Safety
        ]
