module PageWrapper exposing (..)

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
import Nested exposing (modifyCmd)


type Msg
    = Error Err.Msg
    | EventName Event.Msg


type alias HasPageWrapper m =
    BreezeApi.HasBreezeApi (Event.HasEventName m)


initPages : HasPageWrapper m -> ( HasPageWrapper m, Cmd Msg )
initPages mdl =
    ( mdl, Cmd.map (always (EventName Event.GetEventName)) Cmd.none )


update : Msg -> HasPageWrapper m -> ( HasPageWrapper m, Cmd Msg )
update msg mdl =
    case msg of
        Error msg ->
            ( Err.update msg mdl, Cmd.none )

        EventName msg ->
            modifyCmd EventName <| Event.update msg mdl


view : (Msg -> msg) -> HasPageWrapper m -> Html msg -> Html msg
view f mdl main =
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
            [ Html.map (f << Error) <| Err.view mdl
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

        page =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ main
                    ]
                ]
            ]

        body =
            errors
                |> List.append loading
                |> List.append titleRow
                |> flip List.append page
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
