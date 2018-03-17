module Pages.SearchPage exposing (..)

import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FindPeople as Find
import Html as Html exposing (Html, div, h2, text, h4)
import Html.Attributes exposing (class)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Search
    , nextPageRoute = Router.Selected
    , pageTitle = "Search"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h4 [] [ text "Let's Start By Finding Your Family" ]
                    ]
                ]

        searchForm =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ Html.map FindMsg <| Find.searchPersonsForm mdl
                    ]
                ]
    in
    div [ class "d-flex flex-column justify-content-around align-items-center h-100"]
        [ div [class "order-1 grow-1 d-flex align-items-center"] [title]
        , div [class "order-2 grow-6"] [searchForm]
        ]
