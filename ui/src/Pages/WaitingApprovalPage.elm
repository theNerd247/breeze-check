module Pages.WaitingApprovalPage exposing (..)

import FindPeople as Find
import Html as Html exposing (Html, br, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import Pages exposing (..)
import Person as Person
import Router as Router


config : Config
config =
    { pageRoute = Router.WaitingApproval
    , pageTitle = "Checking In"
    , pageView = view
    , showInNavbar = False
    }


view : Model -> Html Msg
view mdl =
    let
        title =
            div [ class "w-100 text-center grow-1" ]
                [ h2 [] [ text "You're Almost Done!" ]
                , p [ class "text-center" ]
                    [ text "Stop by the check-in desk and give them this number"
                    ]
                ]

        cancelCheckin =
            div [ class "w-100 text-center grow-1 mb-3" ]
                [ Html.map FindMsg <| Find.cancelCheckInButton
                ]

        groupId gid =
            div [ class "w-100 text-center grow-2" ]
                [ h1 [] [ text <| toString gid ]
                ]

        checkInPpl =
            div [ class "w-100 text-center grow-6" ]
                [ h3 [] [ text "You're Checking In" ]
                , br [] []
                , Person.onlyListPersons mdl.waitingCheckIn
                ]

        information =
            div [ class "text-center grow-1" ]
                [ p []
                    [ text
                        """
                  Please make sure your family waits in line with you. They will
                  need stamps to participate.
                  """
                    ]
                , p []
                    [ text
                        """
                        """
                    ]
                ]
    in
    pageWrapper "" <|
        case mdl.groupId of
            Nothing ->
                []

            Just gid ->
                [ title
                , groupId gid
                , cancelCheckin
                , information
                , checkInPpl
                ]
