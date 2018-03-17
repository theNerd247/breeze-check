module Pages.WaitingApprovalPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople as Find
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
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
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h2 [] [ text "You're Almost Done!" ]
                    ]
                ]

        inst =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs10 ]
                    [ p [ class "text-center" ]
                        [ text "Stop by the check-in desk and show them this number"
                        ]
                    ]
                ]

        cancelCheckin =
            Grid.row [ Row.centerXs, Row.attrs [ class "pb-3" ] ]
                [ Grid.col [ Col.xsAuto ]
                    [ Html.map FindMsg <| Find.cancelCheckInButton
                    ]
                ]

        groupId gid =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h1 [] [ text <| toString gid ]
                    ]
                ]

        checkInTitle =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs10 ]
                    [ h3 [] [ text "You're Checking In" ]
                    ]
                ]
    in
    div [] <|
        case mdl.groupId of
            Nothing ->
                []

            Just gid ->
                [ title
                , inst
                , groupId gid
                , cancelCheckin
                , checkInTitle
                , Find.waitingPersons mdl
                ]
