module Pages exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import ErrorMsg as Err
import EventName as Event
import Html as Html
    exposing
        ( Html
        , div
        , h1
        , h2
        , h3
        , h4
        , p
        , text
        )
import Html.Attributes exposing (class, for, style)


type alias HasMainPage m =
    BreezeApi.HasBreezeApi (Event.HasEventName m)


pageWrapper : HasMainPage m -> (Err.Msg -> msg) -> Html msg -> Html msg
pageWrapper mdl f main =
    let
        titleRow =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [] [ text mdl.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            ]

        errors =
            [ Html.map f <| Err.view mdl
            ]

        loading =
            if mdl.loadingStatus then
                [ Grid.row [ Row.centerXs ]
                    [ Grid.col [ Col.xs12 ]
                        [ loadingBar
                        ]
                    ]
                ]
            else
                []

        body =
            errors
                |> List.append loading
                |> List.append titleRow
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.containerFluid [ class "mb-5" ] body
        ]


loadingBar : Html msg
loadingBar =
    Progress.progress
        [ Progress.value 100
        , Progress.animated
        ]
