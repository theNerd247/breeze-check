module Pages.SelectPage exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople as Find
import Html as Html exposing (Html, div, h2, h3, h4, text)
import Html.Attributes exposing (class, for, style)


selectPageView : Find.HasFind m -> (Find.Msg -> msg) -> Html msg
selectPageView mdl f =
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
                    [ Html.map f <| Find.foundPeopleView mdl
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
                    [ Html.map f <| Find.waitingCheckInView mdl
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
