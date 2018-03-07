module Pages.SearchPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FindPeople as Find
import Html as Html exposing (Html, div, h2, text)


searchPageView : Find.HasFind m -> (Find.Msg -> msg) -> Html msg
searchPageView mdl f =
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
                    [ Html.map f <| Find.searchPersonsForm mdl
                    ]
                ]
    in
    div []
        [ title
        , searchForm
        ]
