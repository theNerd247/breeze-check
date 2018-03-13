module Pages.SelectPage exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, text)
import Html.Attributes exposing (class, for, style)
import Pages as Pages


update =
    Pages.update


view : HasSelectPage m -> Html Pages.Msg
view mdl =
    let
        hasSelectablePeople =
            Dict.size mdl.foundPeople

        title =
            case hasSelectablePeople of
                0 ->
                    text ""

                _ ->
                    Grid.row [ Row.centerXs ]
                        [ Grid.col [ Col.xs12 ]
                            [ h4 [ class "text-center" ] [ text "Select Your Family Members" ]
                            ]
                        ]

        results =
            Grid.row [ Row.attrs [ class "pb-3" ], Row.centerXs ]
                [ Grid.col [ Col.xs12 ] <|
                    [ Html.map FindMsg <| Find.searchResultsView mdl
                    ]
                ]
    in
    div []
        [ Html.map FindMsg <| Find.searchPersonsForm mdl
        , title
        , results
        , continue <| Dict.size mdl.waitingCheckIn <= 0
        ]
