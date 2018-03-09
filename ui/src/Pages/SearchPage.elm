module Pages.SearchPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FindPeople as Find
import Html as Html exposing (Html, div, h2, text)
import Router as Router


type alias Msg =
    Find.Msg


type alias HasSearchPage m =
    Find.HasFind (Router.HasRoutes m)


update : Msg -> HasSearchPage m -> ( HasSearchPage m, Cmd Msg )
update msg mdl =
    Find.update msg <|
        Find.afterSearch msg mdl <|
            Router.setRoute mdl Router.Selected


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
