module Pages.SearchPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FindPeople as Find
import Html as Html exposing (Html, div, h2, text)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Search
    , nextPageRoute = Router.Selected
    , pageTitle = "Search"
    , pageView = view
    }


view : Model -> Html Msg
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
                    [ Html.map FindMsg <| Find.searchPersonsForm mdl
                    ]
                ]
    in
    div []
        [ title
        , searchForm
        ]
