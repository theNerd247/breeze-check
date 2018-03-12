module Pages.SelectPage exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dict as Dict
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, text)
import Html.Attributes exposing (class, for, style)
import Nested exposing (modifyCmd)
import Router as Router


type Msg
    = FindMsg Find.Msg
    | GoToPhoto


type alias HasSelectPage m =
    Find.HasFind (Router.HasRoutes m)


update : Msg -> HasSelectPage m -> ( HasSelectPage m, Cmd Msg )
update msg mdl =
    case msg of
        FindMsg msg ->
            modifyCmd FindMsg <| Find.update msg mdl

        GoToPhoto ->
            ( Router.setRoute mdl Router.Photo, Cmd.none )


view : HasSelectPage m -> Html Msg
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
        , continue <| Dict.size mdl.waitingCheckIn
        ]


continue : Int -> Html Msg
continue numSelected =
    case numSelected of
        0 ->
            Html.div [] []

        _ ->
            Grid.row [ Row.centerXs, Row.attrs [ class "align-items-end" ] ]
                [ Grid.col [ Col.xs12 ]
                    [ div [ class "text-center", class "align-text-bottom" ]
                        [ Button.button
                            [ Button.onClick <| GoToPhoto
                            , Button.outlineSuccess
                            , Button.disabled <| numSelected <= 0
                            ]
                            [ text <| "Continue (" ++ toString numSelected ++ ")"
                            ]
                        ]
                    ]
                ]
