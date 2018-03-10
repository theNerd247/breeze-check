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

        results =
            Grid.row [ Row.attrs [ class "pb-3" ], Row.centerXs ]
                [ Grid.col [ Col.xs12 ] <|
                    [ Find.searchResultsView mdl
                    ]
                ]
    in
    div []
        [ title
        , results
        ]
