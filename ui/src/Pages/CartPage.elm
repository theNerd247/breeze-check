module Pages.CartPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, text)
import Html.Attributes exposing (class, for, style)


type alias Msg =
    Find.Msg


type alias HasReviewPage m =
    Find.HasFind m


update : Msg -> HasReviewPage m -> ( HasReviewPage m, Cmd Msg )
update msg mdl =
    Find.update msg mdl


view : HasReviewPage m -> Html Msg
view mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h3 [] [ text "You're checking in" ]
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
    in
    div [] [ title, waiting ]
