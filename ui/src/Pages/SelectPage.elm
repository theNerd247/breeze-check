module Pages.SelectPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, text)
import Html.Attributes exposing (class, for, style)


type alias Msg =
    Find.Msg


type alias HasSelectPage m =
    Find.HasFind m


update : Msg -> HasSelectPage m -> ( HasSelectPage m, Cmd Msg )
update =
    Find.update


view : HasSelectPage m -> Html Msg
view mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ h4 [ class "text-center" ] [ text "Select Your Family Members" ]
                    ]
                ]

        found =
            Grid.row [ Row.attrs [ class "pb-3" ], Row.centerXs ]
                [ Grid.col [ Col.xs12 ] <|
                    [ Find.foundPeopleView mdl
                    ]
                ]

        waiting =
            let
                a =
                    if not <| List.isEmpty mdl.waitingCheckIn then
                        [ class "pb-3" ]
                    else
                        []
            in
            Grid.row [ Row.attrs a, Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ Find.waitingCheckInView mdl
                    ]
                ]

        header =
            Grid.row [ Row.centerXs ] <|
                if not <| List.isEmpty mdl.waitingCheckIn then
                    [ Grid.col [ Col.xsAuto ]
                        [ h3 [] [ text "You're checking in" ]
                        ]
                    ]
                else
                    []
    in
    div []
        [ title
        , found
        , header
        , waiting
        ]
