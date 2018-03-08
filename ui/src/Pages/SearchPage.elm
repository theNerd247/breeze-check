module Pages.SearchPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FindPeople as Find
import Html as Html exposing (Html, div, h2, text)


type alias Msg =
    Find.Msg


type alias HasSearchPage m =
    Find.HasFind m


update : Msg -> HasSearchPage m -> ( HasSearchPage m, Cmd Msg )
update =
    Find.update


view : HasSearchPage m -> Html Msg
view mdl =
    let
        title =
            Grid.row []
                [ Grid.col [ Col.xsAuto ]
                    [ h2 [] [ text "Find Your Family" ]
                    ]
                ]

        searchForm =
            Grid.row []
                [ Grid.col [ Col.xs12 ]
                    [ Find.searchPersonsForm mdl
                    ]
                ]
    in
    div []
        [ title
        , searchForm
        ]
